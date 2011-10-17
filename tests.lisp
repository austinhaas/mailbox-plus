(in-package #:mailbox-plus-tests)

(defun run-test (test-spec)
  "Same as run!, but adds an extra message."
  (format t "~&Testing: ~S" test-spec)
  (run! test-spec))

(defun make-linear-int-gen (&optional (start 0))
  (let ((cur start))
    (lambda ()
      (prog1
          cur
        (incf cur)))))

(defun start-auto-sender (m+ &key (max-messages 5) (initial-delay 0) (msg-interval 100))
  (let ((g (make-linear-int-gen)))
    (sb-thread:make-thread (lambda ()
                             (sleep (/ initial-delay 1000))
                             (dotimes (i max-messages)
                               (send-message m+ (funcall g))
                               (sleep (/ msg-interval 1000)))))))

(defun always-true (item)
  (declare (ignore item))
  t)

(defmacro good-recv (desired exp)
  `(is (equal (list ,desired t) (multiple-value-list ,exp))))

(defmacro bad-recv (exp)
  `(is (equal (list nil nil) (multiple-value-list ,exp))))

(def-suite basic-suite :description "Basic suite.")

(in-suite basic-suite)

(test send
  (let ((m+ (make-mailbox-plus :name "test-send-mailbox")))
    (finishes (send-message m+ 1))
    (good-recv 1 (receive-message m+))))

(test receive
  (let ((m+ (make-mailbox-plus :name "test-receive-mailbox" :initial-contents (list 1 2 3))))
    (good-recv 1 (receive-message m+))
    (good-recv 2 (receive-message m+))
    (good-recv 3 (receive-message m+))
    (bad-recv (receive-message m+))))

(test receive-with-timeout
  (let ((m+ (make-mailbox-plus :name "test-receive-with-timeout-mailbox")))
    (start-auto-sender m+ :initial-delay 100 :msg-interval 100 :max-messages 3)
    (good-recv 0 (receive-message m+ 200 50))
    (good-recv 1 (receive-message m+ 200 50))
    (bad-recv (receive-message m+ 10 50))
    (good-recv 2 (receive-message m+))
    (bad-recv (receive-message m+))))

(test receive-if
  (let ((m+ (make-mailbox-plus :name "test-receive-if-mailbox")))
    (start-auto-sender m+ :initial-delay 100 :msg-interval 100 :max-messages 2)
    (good-recv 1 (receive-message-if m+ #'oddp 200 50))
    (good-recv 0 (receive-message m+))))

(defun run-tests ()
  (run-test 'basic-suite))
