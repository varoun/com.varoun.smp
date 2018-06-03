(in-package #:com.varoun.concurrency)

;; Condition variables.
;; As compared to bordeaux-threads, this implementation adds condition-notify-all method

(defclass condition-variable ()
  ((condition :initarg :condition
              :reader condition-condition
              :documentation "Underlying bordeaux-thread condition object.")
   (lock :initarg :lock
         :reader condition-lock
         :documentation "Lock to protect waiting-thread-count")
   (waiting-thread-count :initform 0
                         :accessor condition-waiting-thread-count
                         :documentation
                         "Number of threads waiting on the condition (needed to implement
                         condition-notify-all).")))

(defun make-condition-variable (&key name)
  "Creates condition variables.
   NAME -- string name, used for debugging."
  (make-instance 'condition-variable
    :condition (bt:make-condition-variable :name name)
    :lock (make-lock :name
                     (format nil "Internal condition-variable lock for ~A" name))))

(defun condition-wait (condition lock)
  "Atomically releases the LOCK and blocks the thread waiting on the condition.
   This function returns when another thread calls `condition-notify' or `condition-notify-all'.
   Upon return the LOCK is re-acquired.
   It is an error to call this function if LOCK is not currently held by the thread, or if lock is a
   reentrant lock."
  (check-type condition condition-variable)
  (check-type lock lock)
  (assert (lock-held-by-current-thread-p lock))
  (with-lock-held ((condition-lock condition))
    (incf (condition-waiting-thread-count condition)))
  (unwind-protect
       (progn
         (mark-lock-released-before-condition-wait lock)
         (bt:condition-wait (condition-condition condition)
                            (lock-main-lock lock))
         (mark-lock-acquired-after-condition-wait lock))
    (with-lock-held ((condition-lock condition))
      (decf (condition-waiting-thread-count condition)))))

(defun condition-notify (condition)
  "Wakes up at least one waiting thread."
  (check-type condition condition-variable)
  (bt:condition-notify (condition-condition condition)))

(defun condition-notify-all (condition)
  "Wakes up all waiting threads."
  (check-type condition condition-variable)
  (with-lock-held ((condition-lock condition))
    (dotimes (i (condition-waiting-thread-count condition))
      (bt:condition-notify (condition-condition condition)))
    (setf (condition-waiting-thread-count condition) 0)))
