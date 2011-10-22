(in-package #:mailbox-plus)

(defparameter *default-timeout* nil
  "The default amount of time, in milliseconds, that an operation will
wait for a new message before giving up, or nil, if the operation
should wait forever.")
(defparameter *sleep-interval* 200
  "The default interval, in milliseconds, that a function will sleep
between repeated attempts to retrieve the next message.")

(define-condition mailbox-plus-timeout-condition (serious-condition)
  ((mailbox :initarg mailbox :reader mailbox)
   (timeout :initarg timeout :reader timeout))
  (:documentation "Signaled when an receive operation times out."))

(defun receive-message-with-timeout (mailbox timeout)
  "Tries to retrieve a message from mailbox, blocking until timeout if
necessary. timeout can either be nil, which means to wait forever, or
an integer specifying the amount of time to wait (in milliseconds). A
negative timeout value is the same as 0. A
mailbox-plus-timeout-condition is signaled if a message is not
received before timeout.

Two restarts are provided, allowing the operation to continue waiting:

continue-waiting: takes 0 arguments and causes the operation to wait
another timeout milliseconds

continue-waiting-with-new-timeout: takes 1 arguments: a new value for
timeout"
  ;; I'll change this crude implementation once SBCL has better
  ;; support for timeouts.
  (cond ((null timeout)
         (sb-concurrency:receive-message mailbox))
        (t
         (let ((time-remaining timeout))
           (loop
             (multiple-value-bind (message success)
                 (sb-concurrency:receive-message-no-hang mailbox)
               (when success
                 (return-from receive-message-with-timeout message))
               (when (<= time-remaining 0)
                 (restart-case
                     (error 'mailbox-plus-timeout-condition :mailbox mailbox :timeout timeout)
                   (continue-waiting ()
                     :report (lambda (stream) (format stream "Continue waiting another ~A milliseconds." timeout))
                     (setf time-remaining timeout))
                   (continue-waiting-with-new-timeout (timeout)
                     :report "Continue waiting (with new timeout)."
                     :interactive (lambda ()
                                    (format t "~&How long to wait? (in milliseconds): ")
                                    (list (eval (read))))
                     (setf time-remaining timeout))))
               (sleep (/ (min *sleep-interval* time-remaining) 1000))
               (if (> time-remaining *sleep-interval*)
                   (decf time-remaining *sleep-interval*)
                   (setf time-remaining 0))))))))

(defclass mailbox-plus ()
  ((mailbox :type sb-concurrency:mailbox :initarg :mailbox :reader mailbox)
   (buffer :type list :initform nil :accessor buffer)))

(defun make-mailbox-plus (&key name initial-contents)
  "Returns a new mailbox with messages in initial-contents enqueued."
  (make-instance 'mailbox-plus
                 :mailbox (sb-concurrency:make-mailbox :name name
                                                       :initial-contents initial-contents)))

(defun send-message (m+ message)
  "Adds a message to mailbox. Message can be any object."
  (sb-concurrency:send-message (mailbox m+) message))

(defun receive-message (m+ &optional (timeout *default-timeout*))
  "Removes the oldest message from mailbox and returns it. A
mailbox-plus-timeout-condition is signaled if a message could not be
received. If timeout is nil, wait indefinitely for a new message. If
timeout <= 0, check for a new message, but give up immediately if one
wasn't received. If timeout > 0, then check for a new message every
sleep-interval milliseconds, but give up after timeout milliseconds."
  (if (buffer m+)
      (pop (buffer m+))
      (receive-message-with-timeout (mailbox m+) timeout)))

(defun receive-message-if (m+ predicate &optional (timeout *default-timeout*))
  "Like receive-message, but only returns the first message that
satisfies predicate. Any incoming messages arriving before a match is
found will remain in the mailbox in the order they arrived."
  ;; First check buffer.
  (let ((item (find-if predicate (buffer m+))))
    (when item
      (setf (buffer m+) (delete item (buffer m+)))
      (return-from receive-message-if (values item t))))
  ;; Then check incoming messages.
  (let ((mailbox (mailbox m+))
        (non-matching-messages nil)
        (time-remaining timeout))
    (loop
      (let ((msg (receive-message-with-timeout mailbox time-remaining)))
        (cond ((funcall predicate msg)
               (when non-matching-messages
                 (setf (buffer m+) (nconc (nreverse non-matching-messages) (buffer m+))))
               (return-from receive-message-if msg))
              (t
               (decf time-remaining *sleep-interval*)
               (push msg non-matching-messages)))))))
