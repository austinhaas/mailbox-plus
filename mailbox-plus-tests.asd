(asdf:defsystem #:mailbox-plus-tests
  :description "Test suite for mailbox-plus."
  :author "Austin Haas <austin@pettomato.com>"
  :licence "MIT"
  :version "0.1.1"
  :depends-on (#:mailbox-plus
               #:fiveam)
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "tests")))))
