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

(def-suite basic-suite :description "Basic suite.")

(in-suite basic-suite)

(test send
  (let ((m+ (make-mailbox-plus :name "test-send-mailbox")))
    (finishes (send-message m+ 1))
    (is (= 1 (receive-message m+)))))

(test receive
  (let ((m+ (make-mailbox-plus :name "test-receive-mailbox" :initial-contents (list 1 2 3))))
    (is (= 1 (receive-message m+)))
    (is (= 2 (receive-message m+)))
    (is (= 3 (receive-message m+)))
    (signals mailbox-plus-timeout-condition (receive-message m+ 100))))

(test receive-with-timeout
  (let ((m+ (make-mailbox-plus :name "test-receive-with-timeout-mailbox")))
    (start-auto-sender m+ :initial-delay 100 :msg-interval 100 :max-messages 3)
    (signals mailbox-plus-timeout-condition (receive-message m+ 10))
    (is (= 0 (receive-message m+)))
    (is (= 1 (receive-message m+ 100)))
    (is (= 2 (receive-message m+ 100)))
    (signals mailbox-plus-timeout-condition (receive-message m+ 100))))

(test receive-if
  (let ((m+ (make-mailbox-plus :name "test-receive-if-mailbox")))
    (start-auto-sender m+ :initial-delay 100 :msg-interval 100 :max-messages 2)
    (is (= 1 (receive-message-if m+ #'oddp 250)))
    (is (= 0 (receive-message m+)))))

(test restarts
  (let ((m+ (make-mailbox-plus :name "test-receive-if-mailbox")))
    (start-auto-sender m+ :initial-delay 100 :msg-interval 100 :max-messages 2)
    (is (= 0 (handler-bind ((mailbox-plus-timeout-condition
                              (lambda (c)
                                (declare (ignore c))
                                (invoke-restart 'continue-waiting))))
               (receive-message m+ 50))))
    (is (= 1 (handler-bind ((mailbox-plus-timeout-condition
                              (lambda (c)
                                (declare (ignore c))
                                (invoke-restart 'continue-waiting-with-new-timeout 1000))))
               (receive-message m+ 50))))))

(defun run-tests ()
  (run-test 'basic-suite))
