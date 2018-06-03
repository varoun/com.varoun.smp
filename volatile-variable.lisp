(in-package #:com.varoun.concurrency)

(defclass volatile-variable ()
  ((lock :initform (make-lock :name "volatile-variable lock")
         :accessor volatile-variable-lock)
   (value :initarg :value
          :accessor %%volatile-variable-value)))

(defun make-volatile-variable (value)
  "Creates a volatile variable with initial value VALUE.
   All reads of a volatile variable are guaranteed to see all preceding writes."
  (make-instance 'volatile-variable
    :value value))

(defmethod variable-value ((var volatile-variable))
  "Returns current value of the variable reflecting the most-recent write (from any thread)."
  (with-lock-held ((volatile-variable-lock var))
    (%%volatile-variable-value var)))

(defmethod (setf variable-value) (value (var volatile-variable))
  "Updates the value of a volatile variable."
  (with-lock-held ((volatile-variable-lock var))
    (setf (%%volatile-variable-value var) value)))
