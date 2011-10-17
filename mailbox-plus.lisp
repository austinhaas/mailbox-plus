(in-package #:mailbox-plus)

(defparameter *default-timeout* 5000
  "The default amount of time, in milliseconds, that an operation will
wait for a new message before giving up.")
(defparameter *sleep-interval* 200
  "The default interval, in milliseconds, that a function will sleep
between repeated attempts to retrieve the next message.")

(defun receive-message-with-timeout (mailbox timeout &optional (sleep-interval *sleep-interval*))
  "Tries to retrieve a message from mailbox, blocking until timeout if
necessary. Returns two values: the message retrieved, or nil if the
operation timed out, and a boolean indicating if the operation was
successful or not. The value of timeout is specified in
milliseconds. If timeout is nil, then it is assumed to be infinity. A
negative timeout is the same as 0."
  ;; I'll change this crude implementation once SBCL has better
  ;; support for timeouts.
  (cond ((null timeout)
         (values (sb-concurrency:receive-message mailbox) t))
        ((<= timeout 0)
         (sb-concurrency:receive-message-no-hang mailbox))
        (t
         (let ((time-remaining timeout))
           (loop
             (multiple-value-bind (message success)
                 (sb-concurrency:receive-message-no-hang mailbox)
               (when success
                 (return (values message t)))
               (when (<= time-remaining 0)
                 (return (values nil nil)))
               (sleep (/ (min sleep-interval time-remaining) 1000))
               (if (> time-remaining sleep-interval)
                   (decf time-remaining sleep-interval)
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

(defun receive-message (m+ &optional (timeout *default-timeout*) (sleep-interval *sleep-interval*))
  "Removes the oldest message from mailbox and returns it as the
primary value. A secondary value indicates if a message could be
received. If timeout is nil, wait indefinitely for a new message. If
timeout <= 0, check for a new message, but give up immediately if one
wasn't received. If timeout > 0, then check for a new message every
sleep-interval milliseconds, but give up after timeout
milliseconds."
  (if (buffer m+)
      (values (pop (buffer m+)) t)
      (receive-message-with-timeout (mailbox m+) timeout sleep-interval)))

(defun receive-message-if (m+ predicate &optional (timeout *default-timeout*) (sleep-interval *sleep-interval*))
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
    (flet ((save-messages ()
             (when non-matching-messages
               (setf (buffer m+) (nconc (nreverse non-matching-messages) (buffer m+))))))
      (loop
        (multiple-value-bind (msg success) (receive-message-with-timeout mailbox time-remaining sleep-interval)
          (cond ((not success)
                 (save-messages)
                 (return-from receive-message-if (values nil nil)))
                ((funcall predicate msg)
                 (save-messages)
                 (return-from receive-message-if (values msg t)))
                (t
                 (decf time-remaining sleep-interval)
                 (push msg non-matching-messages))))))))
