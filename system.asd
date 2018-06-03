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
  :components ((:module :decls
                        :pathname #P"."
                        :serial t
                        :components ((:file "pkgdcl")))
               (:module :main
                        :pathname #p"."
                        :serial t
                        :components ((:file "threads")
                                     (:file "thread-variables" :depends-on ("threads"))
                                     (:file "locking")))))
