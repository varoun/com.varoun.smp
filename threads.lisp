;;;; Portable thread functions

(in-package #:com.varoun.threads)

;;; Most of these are simple wrappers around bordeaux-thread functions

;; Thread-local bindings
(defvar *thread-variable-bindings* nil
  "A list of (list var-name initial-value-fn).")

(defvar *thread-variable-names* nil
  "List of all thread-variable names computed from *thread-variable-bindings*.
   This is just an optimization to store variables as they are accepted by progv.")

(defvar *thread-variable-value-fns* nil
  "List of all thread-variable value functions computed from *thread-variable-bindings*.
   This is just an optimization to store variables as they are accepted by progv.")

(defun compute-thread-variable-lists ()
  "Caches separate name and value lists of *thread-variable-bindings* for faster access when
creating a new thread."
  (setf *thread-variable-names* (mapcar #'first *thread-variable-bindings*))
  (setf *thread-variable-value-fns* (mapcar #'second *thread-variable-bindings*)))

(defun add-thread-variable-binding (var-name initial-value-fn)
  "Registers a thread-local variable binding: VAR-NAME is the name of a special variable, and
  INITIAL-VALUE-FN is a function that returns that variable's initial value in a new thread."
  (check-type var-name symbol)
  (check-type initial-value-fn (or null function symbol))
  (let ((cell (assoc var-name *thread-variable-bindings*)))
    (unless cell
      (setf cell (cons var-name nil))
      (push cell *thread-variable-bindings*))
    (setf (cdr cell) (list initial-value-fn)))
  (compute-thread-variable-lists)
  (values))

(defun make-thread-function-wrapper ()
  "Create a function wrapper to inherit the values from current thread"
  (let ((values (mapcar #'funcall *thread-variable-value-fns*)))
    #'(lambda (function)
        (progv *thread-variable-names* values
          (funcall function)))))

(defun wrap-thread-function (function)
  (let ((w (make-thread-function-wrapper)))
    #'(lambda () (funcall w function))))

(defun make-thread (function &key name)
  "Creates and starts a thread.
   FUNCTION -- the function executed in the new thread.
   NAME -- optional name of a thread.
   Returns created thread object."
  (bt:make-thread (wrap-thread-function function)
                  :name name
                  ;; don't use bt initial-bindings, because bt:make-thread evals the initial-value
                  ;; form at run-time
                  :initial-bindings nil))

(defun join-thread (thread)
  "Waits until THREAD terminates and returns values returned by the THREAD's execution function.
   NOTE: not all Lisp implementation return these values, but SBCL and CCL do."
  (bt:join-thread thread))

(defun current-thread ()
  "Returns current thread object."
  (bt:current-thread))

(defun thread-name (thread)
  "Returns name of the THREAD."
  (bt:thread-name thread))

(defun all-threads ()
  "Returns a list of all currently executing threads."
  (bt:all-threads))

(defun interrupt-thread (thread function)
  "Interrupts a thread and executes FUNCTION in this THREAD."
  (bt:interrupt-thread thread function))

(defun thread-alive-p (thread)
  "Returns true if THREAD is still executing."
  (bt:thread-alive-p thread))
