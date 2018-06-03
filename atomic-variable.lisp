(in-package #:com.varoun.concurrency)

(defclass atomic-variable ()
  ((lock :initform (make-lock :name "atomic-variable lock")
         :accessor atomic-variable-lock
         :documentation "Lock protecting access to the variable's value.")
   (value :initarg :value
          :accessor %%atomic-variable-value
          :documentation "Variable's value.")))

(defun make-atomic-variable (value)
  "Create an atomic variable with the given value."
  (make-instance 'atomic-variable
    :value value))

(defmethod variable-value ((var atomic-variable))
  "Returns variable's value."
  (with-lock-held ((atomic-variable-lock var))
    (%%atomic-variable-value var)))

(defmethod (setf variable-value) (value (var atomic-variable))
  "Sets variable's value."
  (with-lock-held ((atomic-variable-lock var))
    (setf (%%atomic-variable-value var) value)))

(defmethod mutate-variable ((var atomic-variable) mutation-fn)
  "Atomically apply MUTATION-FN to the variable's value, and set the value to the result."
  (with-lock-held ((atomic-variable-lock var))
    (setf (%%atomic-variable-value var)
          (funcall mutation-fn (%%atomic-variable-value var)))))

(defmethod increment-variable ((var atomic-variable) &optional (delta 1))
  "Atomically increments the value by DELTA (assumes that value is an integer)."
  (mutate-variable var #'(lambda (v) (incf v delta))))
