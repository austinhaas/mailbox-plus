(asdf:defsystem #:mailbox-plus
  :serial t
  :depends-on (#:sb-concurrency)
  :components ((:file "package")
               (:file "mailbox-plus")))
