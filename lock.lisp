(in-package #:com.varoun.concurrency)

;; There are two advantages of this lock implementation over the bordeaux-thread implementation:
;; 1) it implements lock-held-by-current-thread-p predicate, which is very useful for assertions
;; 2) it makes sure that non-reentrant locks are not obtained more than once (this matters for CCL,
;; which only has reentrant locks, and the bordeaux-thread implementation just uses them for
;; non-reentrant locks without any extra logic)

(defclass lock ()
  ((main-lock :initarg :main-lock
              :accessor lock-main-lock
              :documentation "Bordeaux-thread lock object representing the actual lock")
   (obj-lock :initarg :obj-lock
             :reader lock-obj-lock
             :documentation "Bordeaux-thread (non-reentrant) lock object protecting/synchronizing
             the state of this lock object (the owner and owner-hold-count slots).")
   (reentrant-p :initarg :reentrant-p
                :reader lock-reentrant-p
                :documentation "Is this lock reentrant?")
   (name :initform "unknown lock"
         :initarg :name
         :reader lock-name
         :documentation "Name assigned to this lock at creation time.")
   (owner :initform nil
          :accessor %%lock-owner
          :documentation "Returns thread that is helding this lock")
   (owner-hold-count :initform 0
                     :accessor %%lock-owner-hold-count
                     :documentation
                     "Returns number of holds of this lock by the owner thread (>1 for reentrant
                     locks).")))

(defun make-lock (&key name reentrant-p)
  "Creates a lock.
   NAME -- string description of the lock (useful for debugging)
   REENTRANT-P -- indicates if the lock should be reentrant (ie can be acquired multiple times by
   the same thread)"
  (make-instance 'lock
    :main-lock (if reentrant-p
                 (bt:make-recursive-lock name)
                 (bt:make-lock name))
    :obj-lock (bt:make-lock (format nil "Internal lock protecting ~A lock" name))
    :reentrant-p reentrant-p
    :name name))

(defun acquire-lock (lock &key (wait-p t))
  "Acquires LOCK.
   If wait-p is false, then returns NIL if the lock can't be acquired immediately, or acquires the
   lock and returns T.
   If wait-p is true, then blocks indefinitely until the lock can be acquired (returning T)."
  (check-type lock lock)
  (let ((locked-p (bt:acquire-lock (lock-main-lock lock) wait-p)))
    (when locked-p
      (bt:with-lock-held ((lock-obj-lock lock))
        (cond
          ((%%lock-owner lock)
           (unless (lock-reentrant-p lock)
             (bt:release-lock (lock-main-lock lock))
             (assert-bug "attempt to acquire non-reentrant lock on a held lock ~A" (lock-name lock)))
           (incf (%%lock-owner-hold-count lock)))
          (t
           (setf (%%lock-owner lock) (bt:current-thread))
           (setf (%%lock-owner-hold-count lock) 1)))))
    locked-p))

(defun release-lock (lock)
  (check-type lock lock)
  (bt:with-lock-held ((lock-obj-lock lock))
    (assert (eq (%%lock-owner lock) (bt:current-thread))
            () "attempt to release lock ~A, when it is not held by the current thread" lock)
    (bt:release-lock (lock-main-lock lock))
    (decf (%%lock-owner-hold-count lock))
    (when (zerop (%%lock-owner-hold-count lock))
      (setf (%%lock-owner lock) nil))))

(defun mark-lock-released-before-condition-wait (lock)
  "Internal function to fix the LOCK state before the underlying lock is released upon
   entering conc:condition-wait"
  (check-type lock lock)
  (assert (not (lock-reentrant-p lock))
          () "can not call condition-wait on a reentrant lock")
  (bt:with-lock-held ((lock-obj-lock lock))
    (assert (eq (%%lock-owner lock) (bt:current-thread))
            () "attempt to call condition-wait with lock ~A, when the lock is not held by the
            current thread" lock)
    (setf (%%lock-owner lock) nil)
    (setf (%%lock-owner-hold-count lock) 0)))

(defun mark-lock-acquired-after-condition-wait (lock)
  "Internal function to fix the LOCK state after the underlying lock has been re-acquired upon
   exiting conc:condition-wait"
  (check-type lock lock)
  (assert (not (lock-reentrant-p lock))
          () "can not call condition-wait on a reentrant lock")
  (bt:with-lock-held ((lock-obj-lock lock))
    (assert (null (%%lock-owner lock))
            () "lock is held by thead ~A (%%lock-owner lock) after condition-wait")
    (setf (%%lock-owner lock) (bt:current-thread))
    (setf (%%lock-owner-hold-count lock) 1)))

(defmacro with-lock-held ((lock) &body body)
  "Acquires LOCK while executing BODY, releasing it automatically when BODY exits."
  `(progn
     (acquire-lock ,lock)
     (unwind-protect
          (progn ,@body)
       (release-lock ,lock))))

(defmacro with-lock-released ((lock) &body body)
  "Releases LOCK while executing BODY, re-acquiring it automatically when BODY exits."
  `(progn
     (assert (not (lock-reentrant-p ,lock))
             () "can not use with-lock-released on a reentrant lock")
     (release-lock ,lock)
     (unwind-protect
          (progn ,@body)
       (acquire-lock ,lock))))

(defun lock-owner (lock)
  "Returns two values:
   1) Thread holding the lock (or nil if lock is not currently held)
   2) Number of times the owner thread is holding the lock (>1 for reentrant locks; 0 if lock is not
   held)."
  (check-type lock lock)
  (bt:with-lock-held ((lock-obj-lock lock))
    (values (%%lock-owner lock)
            (%%lock-owner-hold-count lock))))

(defun lock-held-by-current-thread-p (lock)
  "Returns true if the lock is held by the current thread."
  (check-type lock lock)
  (bt:with-lock-held ((lock-obj-lock lock))
    (eq (%%lock-owner lock) (bt:current-thread))))
