;;;; Code to deal with locking and threads.

(in-package #:com.varoun.smp)

(defmacro without-interrupts (&body body)

  "While in the dynamic scope of this form, attempts
   to process-interrupt this process are deferred
   until this form is exited.  Note: This has
   nothing to do with trying to make this the only
   running process!"
  #+ccl
  `(ccl:without-interrupts ,@body)
  #+sbcl
  `(sb-sys:without-interrupts ,@body))

;;--- TODO(varoun): There are nested eval-when's here, collapse them!
;;; atomic-incf
(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro atomic-incf (place &optional (delta 1))
  "Atomically sets PLACE to (1+ PLACE). NB: There is no guarantee on the number of times PLACE will
be evaluated. For now PLACE must be a struct accessor whose type is T. Returns the new value of
PLACE."
  (let ((new (gensym "NEW-"))
        (old (gensym "OLD-")))
    `(loop
       for ,old = ,place
       for ,new = (+ ,old ,delta)
       until (compare-and-swap ,place ,old ,new)
       finally (return ,new))))

;;; compare-and-swap

(defmacro compare-and-swap (&environment env place old new)
  "Sets PLACE to NEW if its current value is OLD. Returns T if the values was changed or NIL if it
wasn't.

Caveat: PLACE, OLD and NEW may be evaluated multiple times (or NEW may not be evaluated at all). No
guarantees are made on the order of evaluation. But hey, it's fast!"
  (let ((place (macroexpand place env)))
    #+ccl
    (cond
      ((and (consp place) (eq 'cl:car (car place)))
       `(ccl::%rplaca-conditional ,(second place) ,old ,new))
      ((and (consp place) (eq 'cl:cdr (car place)))
       `(ccl::%rplacd-conditional ,(second place) ,old ,new))
      (t
       `(ccl::conditional-store ,place ,old ,new)))
    #+sbcl
    (progn
      (when (symbolp place)
        (setf place `(symbol-value ',place)))
      `(eq (sb-ext:compare-and-swap ,place ,old ,new) ,old))))

;;; make-lock
;;; Make and return a lock with the given name.

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; make-lock can be called at compile-time.
(defun make-lock (name)
  "Make and return a lock with the given name"
  (bordeaux-threads:make-lock name))
) ;; end EVAL-WHEN

;;; name-of-lock
;;; Return the name of this lock.

(defun name-of-lock (lock)
  (declare (ignorable lock))
  #+ccl (ccl:lock-name lock)
  #-ccl (format nil "~A" lock))

;;; with-lock-counting and report-lock-counts
;;; Support for the lock counting feature.

#+count-locks
(defvar *lock-count-table* (make-hash-table)
  "A map from lock objects to the number of times the lock has been locked, nil if never")

;;; This function is only intended to be called manually during debugging.
#+count-locks
(defun report-lock-counts ()

  "For each lock that has ever been held, print the number of times it has been held."

  (maphash #'(lambda (lock count)
               (format t "~&~6D~10T~A~%" count (name-of-lock lock)))
           *lock-count-table*))

;;; The lock-debugging logging file is "/tmp/lockdebug.txt".
(defmacro to-debug-lock-stream (format-control &rest format-args)

  "Format the args to the log file for lock debugging."

  (let ((stream-var (gensym "STREAM")))
    `(with-open-file (,stream-var "/tmp/lockdebug.txt"
                      :direction :output :if-exists :append :if-does-not-exist :create)
       (format ,stream-var ,format-control ,@format-args))))


;;; This is the external form for execution with a lock held.
;;; with-lock-held and with-lock-held-recursive

(defmacro with-lock-held ((lock &optional whostate) &body body)

  "Evaluate the body with the specified lock held, with the given whostate"

  (expand-lock-body lock whostate body nil))

(defmacro with-recursive-lock-held ((lock &optional whostate) &body body)

  "Evaluate the body with the specified lock held, with the given whostate,
   allowing recursive locking"

  (expand-lock-body lock whostate body t))


;;; This is the first stage of expansion of the with-lock-held
;;; macro, which implements the lock count feature if that
;;; feature is enabled at compile time.

#+count-locks
(eval-when (:compile-toplevel :load-toplevel :execute)
(defun expand-lock-body (lock whostate body recursive-p)

  "Use *lock-count-table* to count the number of times this lock is locked.
   Then proceed with expansion."

  (let ((vlock (gensym "LOCK-FOR-COUNT")))
    `(let ((,vlock ,lock))
       (if (gethash ,vlock *lock-count-table*)
	 (incf (gethash ,vlock *lock-count-table*))
	 (setf (gethash ,vlock *lock-count-table*) 1))
       ,(expand-lock-body-1 vlock whostate body recursive-p))))
);eval-when

#-count-locks
(eval-when (:compile-toplevel :load-toplevel :execute)
(defun expand-lock-body (lock whostate body recursive-p)

  "Don't do counting, just proceed with expansion."

  (let ((vlock (gensym "LOCK-FOR-COUNT")))
    `(let ((,vlock ,lock))
       ,(expand-lock-body-1 vlock whostate body recursive-p))))
);eval-when


;;; Next phase of macro expansion.  Add the feature that provides the
;;; developer with debug-time information that helps understand why
;;; threads are waiting, esp. deadlocks.
(eval-when (:compile-toplevel :load-toplevel :execute)

  (define-condition current-thread-unknown (error)
    ()
    (:documentation "There is no way to determine the current thread."))

  (defun expand-lock-body-1 (vlock whostate body recursive-p)

    "Keep track of lock waiting status when we are debugging locking,
   if the thread supports current-thread,
   and inside that proceed with expansion."

    (let ((current-thread-var (gensym "CURRENT-THREAD")))
      (handler-case
          `(let ((,current-thread-var (current-thread)))
             ;; Note that this thread is trying to grab this lock.
             (debug-locking-try-to-grab ,current-thread-var ,vlock)
             (unwind-protect
                  ,(expand-lock-body-2
                    vlock
                    whostate
                    `( ;; This thread is no longer trying to grab this lock.
                      ;; This thread now owns this lock.
                      (when-debugging-locking
                        (setf (gethash ,current-thread-var *debug-lock-thread-wants*) nil)
                        (push ,vlock (gethash ,current-thread-var *debug-lock-thread-owns*))
                        (to-debug-lock-stream "Thread ~A has locked ~A~%"
                                              ,current-thread-var (name-of-lock ,vlock)))
                      (unwind-protect
                           (progn
                             ,@body)
                        ;; This thread is about to relinquish ownership of this lock.
                        (when-debugging-locking
                          (pop (gethash ,current-thread-var *debug-lock-thread-owns*))
                          (to-debug-lock-stream "Thread ~A releasing lock ~A~%"
                                                ,current-thread-var (name-of-lock ,vlock)))))
                    recursive-p)
               ;; This thread no longer owns this lock.
               (when-debugging-locking
                 (to-debug-lock-stream "Thread ~A released lock ~A~%"
                                       ,current-thread-var (name-of-lock ,vlock)))))
        (current-thread-unknown ()
          (expand-lock-body-2 vlock whostate body recursive-p)))))

  ;; Final phase of macro expansion: the implementation-dependent
  ;; locking primitive.

  (defun expand-lock-body-2 (lock whostate body recursive-p)
    (declare (ignore whostate))
    `(,(if recursive-p 'bt:with-recursive-lock-held 'bt:with-lock-held)
       (,lock)
       ,@body)));eval-when

;;; The following variables maintain state for the lock debugging feature.

(defvar *debug-locking* nil

  "If true, (with-lock-held ...) collect debugging information.
   Use (display-thread-lock-state) to see the current set of resources held and waited-on.
   You will also get locking history appended to a file named /tmp/lockdebug.txt -- this
   is useful if lock screwage renders your lisp unusable.
   To make sure this works, make sure to set it before you start threads you want to keep
   track of.")

(defvar *debug-lock-lock* (make-lock "debug-lock lock")
  "Protects the debug-lock-thread hash tables.")

(defvar *debug-lock-thread-wants* (make-hash-table)
  "Keep track of which threads want which resources.
   Maps from a thread to the lock it is waiting for.
   Protected by debug-lock-lock.")

(defvar *debug-lock-thread-owns* (make-hash-table)
  "Keep track of what resources a given thread has locked.
   Maps from a thread to a list of the locks that it currently holds.
   Protected by debug-lock-lock.")

(defvar *debug-lock-pairs* (make-hash-table :test 'equal)
  "The key is (lock1 lock2) and the value is t.
   Each entry means that we have seen an occasion
   when we held lock1 and attempted to lock lock2.
   This is for duplicate elimination, so that we
   only report on each pair once. It makes no
   difference which thread it happened in.")

;;; This is only intended to be called manually during a debugging session.
(defmacro when-debugging-locking (&body body)

  "Perform the body if we are debugging locking, and do the
   body with the debug-lock held."

  `(when *debug-locking*
     (bt:with-lock-held (*debug-lock-lock*)
       ,@body)))

;;; Convenience macro, to make sure we don't forget to hold the debug-lock.
(defun display-thread-lock-state (&optional (stream *standard-output*))

  "Print each threads that is waiting for a lock (and the lock),
   and each thread that is holding locks (and the locks).
   You can examine this to find deadlocks.
   Don't forget that dependencies on database locks can create
   deadlocks that are invisible if you're just looking at the output from this function!"

  (with-lock-held (*debug-lock-lock*)
    (format stream "Threads waiting for locks:~%")
    (loop for thread being the hash-keys in *debug-lock-thread-wants* using (hash-value lock) do
      (when lock
        (format stream "Thread ~A is waiting for ~A~%" thread lock)
        (when (gethash thread *debug-lock-thread-owns*)
          (format stream "        and holds ~A~%"
                  (map 'list #'name-of-lock (gethash thread *debug-lock-thread-owns*))))))
    (format stream "~%~%Threads hold locks but aren't waiting for locks:~%")
    (loop for thread being the hash-keys in *debug-lock-thread-owns* using (hash-value locks) do
      (when locks
        (unless (gethash thread *debug-lock-thread-wants*)
          (format stream "Thread ~A holds locks ~A~%"
                  thread (map 'list #'name-of-lock locks)))))))

(defun debug-locking-try-to-grab (current-thread lock)

  "This is called when the current thread is trying to grab
   the lock.  This function is separated out from the macro
   just so that the macro generates less code, to prevent
   code bloat.  If we are not debugging locking, it does
   nothing.  Otherwise, it reports the attempt to grab the
   lock, and if the current thread is holding some other
   lock, it reports that with a stack trace, subject to
   duplicate elimination."

  (when-debugging-locking
   (assert (null (gethash current-thread *debug-lock-thread-wants*)))
   (setf (gethash current-thread *debug-lock-thread-wants*) lock)
   (to-debug-lock-stream "Thread ~A is attempting to grab lock ~A~%"
                         current-thread (name-of-lock lock))
   (dolist (owned (gethash current-thread *debug-lock-thread-owns*))
     (let ((two-list (list owned lock)))
       (unless (gethash two-list *debug-lock-pairs*)
         (setf (gethash two-list *debug-lock-pairs*) t)
         (to-debug-lock-stream
          "While holding ~A, attempting to lock ~A:~%~A"
          (name-of-lock owned)
          (name-of-lock lock)
          ;; Temporary
          (ignore-errors nil)))))))


(defun call-with-maybe-lock-held (lock thunk &optional whostate)
  (declare (ignorable whostate))
  (if lock
    (with-lock-held (lock whostate) (funcall thunk))
    (funcall thunk)))

(defmacro with-maybe-lock-held ((lock &optional whostate) &body body)
  `(call-with-maybe-lock-held ,lock (lambda () ,@body) ,whostate))

;;;; * Condition variables

(defun make-condition-variable (name)
  (declare (ignore name))
  (bt:make-condition-variable))

(defun condition-wait (condition lock)
  (bt:condition-wait condition lock))

(defun condition-signal (condition)
  (bordeaux-threads:condition-notify condition))
) ;; end EVAL-WHEN
