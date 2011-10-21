(defpackage #:mailbox-plus
  (:use #:cl)
  (:export
   #:*default-timeout*
   #:*sleep-interval*
   #:mailbox-plus-timeout-condition
   #:continue-waiting
   #:continue-waiting-with-new-timeout
   #:make-mailbox-plus
   #:send-message
   #:receive-message
   #:receive-message-if))
