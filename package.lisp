(defpackage #:mailbox-plus
  (:use #:cl)
  (:export
   #:*default-timeout*
   #:*sleep-interval*
   #:mailbox-plus-timeout-condition
   #:make-mailbox-plus
   #:send-message
   #:receive-message
   #:receive-message-if))
