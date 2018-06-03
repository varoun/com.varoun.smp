;;;; Package declarations.

(in-package #:common-lisp-user)

(defpackage #:com.varoun.smp
  (:nicknames #:smp)
  (:documentation "Multithreading Utilities.")
  (:use #:common-lisp #:com.varoun.utils)

  (:export #:add-thread-variable-binding
           #:make-thread
           #:join-thread
           #:current-thread
           #:thread-name
           #:all-threads
           #:interrupt-thread
           #:thread-alive-p)

  (:export #:define-thread-variable)

  (:export #:without-interrupts
           #:atomic-incf
           #:compare-and-swap
           #:make-lock
           #:name-of-lock
           #:with-lock-held
           #:with-recursive-lock-held
           #:with-maybe-lock-held
           #:when-debugging-locking
           #:make-condition-variable
           #:condition-wait
           #:condition-signal))
