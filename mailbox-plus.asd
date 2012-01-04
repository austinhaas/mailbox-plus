(asdf:defsystem #:mailbox-plus
  :description "A simple layer over SBCL's mailbox implementation that adds a selective receive function (a la Erlang) named receive-message-if."
  :author "Austin Haas <austin@pettomato.com>"
  :licence "MIT"
  :version "0.1.1"
  :depends-on (#:sb-concurrency)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "mailbox-plus")))))
