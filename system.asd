;;; -*- mode: lisp -*-

(asdf:defsystem #:com.varoun.smp
  :name "Multiprocessing Utilities"
  :author "Varoun. P"
  :maintainer ("Varoun. P")
  :licence "MIT"
  :description "Multiprocessing Utilities for CL"
  :long-description "Multiprocessing Utilities for CL"
  :depends-on (#:com.varoun.utils #:bordeaux-threads)
  :serial t
  :components ((:module "declarations"
                        :pathname #P"."
                        :serial t
                        :components ((:file "pkgdcl")))
               (:module "threads"
                        :pathname #p"."
                        :serial t
                        :components ((:file "threads")
                                     (:file "thread-variables" :depends-on ("threads"))
                                     (:file "locking")
                                     (:file "events" :depends-on ("locking"))))
               (:module "concurrency"
                        :pathname #P"."
                        :components ((:file "lock")))))
