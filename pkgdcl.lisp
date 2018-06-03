;;;; Package declarations.

(in-package #:common-lisp-user)

(defpackage #:com.varoun.threads
  (:nicknames #:threads)
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
           #:condition-signal)

  (:export #:event
           #:make-event
           #:event-set
           #:event-check
           #:event-wait))

(defpackage #:com.varoun.concurrency
  (:nicknames #:conc)
  (:use #:common-lisp #:com.varoun.utils)

  ;; lock
  (:export #:make-lock
           #:acquire-lock
           #:release-lock
           #:with-lock-held
           #:with-lock-released
           #:lock-name
           #:lock-owner
           #:lock-held-by-current-thread-p)

  ;; condition-variable
  (export #:make-condition-variable
          #:condition-wait
          #:condition-notify
          #:condition-notify-all))
