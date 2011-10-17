(asdf:defsystem #:mailbox-plus-tests
  :serial t
  :depends-on (#:mailbox-plus
               #:fiveam)
  :components ((:file "test-package")
               (:file "tests")))
