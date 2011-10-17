(defpackage #:mailbox-plus
  (:use #:cl)
  (:export
   #:*default-timeout*
   #:*sleep-interval*
   #:make-mailbox-plus
   #:send-message
   #:receive-message
   #:receive-message-if))
