(defpackage #:mailbox-plus
  (:use #:cl)
  (:export
   #:*default-timeout*
   #:make-mailbox-plus
   #:send-message
   #:receive-message
   #:receive-message-if))
