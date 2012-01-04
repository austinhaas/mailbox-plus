(in-package #:mailbox-plus)

(defparameter *default-timeout* nil
  "The default amount of time, in seconds, that an operation will wait
for a new message before giving up, or nil, if the operation should
wait forever.")

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

(defun receive-message (m+ &key (timeout *default-timeout*))
  "Removes the oldest message from mailbox and returns it as the
primary value, and a secondary value of t. If mailbox is empty waits
until a message arrives.

If timeout is provided, and no message arrives within the specified
interval, returns primary and secondary value of nil."
  (if (buffer m+)
      (pop (buffer m+))
      (sb-concurrency:receive-message (mailbox m+) :timeout timeout)))

(defun receive-message-if (m+ predicate &key (timeout *default-timeout*))
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
        (deadline (when (numberp timeout)
                    (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
        (non-matching-messages nil))
    (flet ((save-messages ()
             (when non-matching-messages
               (setf (buffer m+) (nconc (nreverse non-matching-messages) (buffer m+))))))
      (loop
        (multiple-value-bind (msg success)
            (sb-concurrency:receive-message mailbox :timeout timeout)
          (cond ((and success (funcall predicate msg))
                 (save-messages)
                 (return-from receive-message-if (values msg t)))
                (success
                 (push msg non-matching-messages)
                 (when (numberp deadline)
                   (setf timeout (/ (- deadline (get-internal-real-time))
                                    internal-time-units-per-second))
                   (unless (> timeout 0)
                     (save-messages)
                     (return-from receive-message-if (values nil nil)))))
                (t
                 (save-messages)
                 (return-from receive-message-if (values nil nil)))))))))
