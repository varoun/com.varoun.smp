;;;; Support for thread-local variables.

(in-package #:com.varoun.threads)

;;;
;;; Special variables defined with DEFINE-THREAD-VARIABLE are portable
;;; thread-local, and can be optionally initialized within each thread
;;; by a runtime-evaluated form.
;;;

(defmacro define-thread-variable (name &key (documentation nil documentation-p)
                                  (initial-form nil initial-form-p)
                                  (inherit-binding nil)
                                  (existing-variable nil))
  "Define NAME as a 'thread-local' variable. In each thread, it starts
with INITIAL-FORM, unless INHERIT-BINDING is specified.  INITIAL-FORM
is evaluated each time a new thread is created. If INHERIT-BINDING is
given, the variable inherits its binding from the parent thread.  If
INITIAL-FORM is not specified, or if the symbol is already bound when
this macro is invoked, the symbol's current value is not modified.

   Our thread variables are initialized in a thread context when threads
are created using our functions.  They will NOT be initialized by calls to
native thread creation functions (for example, bt:make-thread), which is
important to keep in mind when using 3rd party libraries which create
threads, such as hunchentoot."

  (assert (or inherit-binding initial-form-p))

  `(progn
     ,(if existing-variable
        `(progn #+openmcl(ccl:record-source-file ',name 'thread-variable))
        `(defvar ,name ,initial-form))
     (add-thread-variable-binding
      ',name
      #'(lambda ()
          ,(cond
             ((and inherit-binding initial-form-p)
              `(if (boundp ',name)
                 ,name
                 ,initial-form))
             (initial-form-p
              initial-form)
             (inherit-binding
              name))))
     ,(when initial-form-p
        `(unless (boundp ',name)
           (setf ,name ,initial-form)))
     ,(when documentation-p
        `(setf (documentation ',name 'variable) ',documentation))
     ',name))

(defmacro define-thread-stream (stream)
  "Make global stream STREAM thread-local."
  `(progn
     (add-thread-variable-binding ',stream
                                  #'(lambda () ,stream))
     ',stream))

(define-thread-stream *error-output*)
(define-thread-stream *standard-output*)
(define-thread-stream *trace-output*)
(define-thread-stream *debug-io*)

(define-thread-variable *random-state* :existing-variable t :initial-form (make-random-state t))
