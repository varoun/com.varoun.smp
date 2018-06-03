;;;; Events

(in-package #:com.varoun.threads)

(defstruct (event (:constructor make-event (&key
                                            name
                                            (waitqueue (make-condition-variable "event"))
                                            (lock (make-lock "lock"))
                                            (value nil))))
  "Represents synchronization primitive 'event', which can be signaled and waited on."
  (name      nil :type t :read-only t)
  (value     nil :type t)
  (waitqueue nil :read-only t)
  (lock      nil :read-only t))

(defmethod print-object ((event event) stream)
  (print-unreadable-object (event stream :type t :identity t)
    (princ " :NAME " stream) (prin1 (event-name event) stream)
    (princ " :VALUE " stream) (prin1 (event-value event) stream)
    (princ " " stream) (princ (event-waitqueue event) stream)))

(defun event-set (event &key (value t))
  "Sets 'value' slot of the EVENT to specified VALUE, and notifies WAKEUP-N waiters."
  (with-lock-held ((event-lock event))
    (setf (event-value event) value)
    (condition-signal (event-waitqueue event)))
  (values))

(defun event-check (event)
  "Returns 'value' slot of the EVENT."
  (event-value event))

(defun event-wait (event &key (value t) (test #'eql))
  "Waits for 'value' slot of the EVENT to satisfy TEST with the specified VALUE.
   Returns new value of the EVENT."
  (with-lock-held ((event-lock event))
    (loop until (funcall test (event-value event) value)
          do (condition-wait (event-waitqueue event) (event-lock event)))
    (event-value event)))
