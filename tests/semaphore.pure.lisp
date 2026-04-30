 (use-package "SB-THREAD")

(with-test (:name :semaphore-multiple-waiters :skipped-on (or (not :sb-thread) :gc-stress))
  (let ((semaphore (make-semaphore :name "test sem")))
    (labels ((make-readers (n i)
               (values
                (loop for r from 0 below n
                      collect
                      (make-thread
                       (lambda ()
                         (sb-ext:with-timeout 10
                           (let ((sem semaphore))
                             (dotimes (s i)
                               (wait-on-semaphore sem)))))
                       :name "reader"))
                (* n i)))
             (make-writers (n readers i)
               (let ((j (* readers i)))
                 (multiple-value-bind (k rem) (truncate j n)
                   (values
                    (let ((writers
                           (loop for w from 0 below n
                                 collect
                                 (make-thread
                                  (lambda ()
                                    (sb-ext:with-timeout 10
                                      (let ((sem semaphore))
                                        (dotimes (s k)
                                          (signal-semaphore sem)))))
                                  :name "writer"))))
                      (assert (zerop rem))
                      writers)
                    (+ rem (* n k))))))
             (test (r w n)
               (multiple-value-bind (readers x) (make-readers r n)
                 (assert (= (length readers) r))
                 (multiple-value-bind (writers y) (make-writers w r n)
                   (assert (= (length writers) w))
                   (assert (= x y))
                   (mapc #'join-thread writers)
                   (mapc #'join-thread readers)
                   (assert (zerop (semaphore-count semaphore)))
                   (values)))))
      (assert
       (eq :ok
           (sb-ext:with-timeout 20
             (test 1 1 100)
             (test 2 2 10000)
             (test 4 2 10000)
             (test 4 2 10000)
             (test 10 10 10000)
             (test 10 1 10000)
             :ok))))))

(with-test (:name (wait-on-semaphore :timeout :many-threads)
            :skipped-on (not :sb-thread))
  (let* ((count 10)
         (semaphore (make-semaphore)))
    ;; Add 10 tokens right away.
    (signal-semaphore semaphore count)
    ;; 100 threads try to decrement the semaphore by 1.
    (let ((threads
           (loop repeat 100
              collect (make-thread
                       (lambda ()
                         (sleep (random 0.02))
                         (wait-on-semaphore semaphore :timeout 0.5))))))
      ;; Add 10 more tokens while threads may already be waiting and
      ;; decrementing.
      (loop repeat (floor count 2) do (signal-semaphore semaphore 2))
      ;; 20 threads should have been able to decrement the semaphore
      ;; and obtain an updated count.
      (let ((values (mapcar #'join-thread threads)))
        ;; 20 threads should succeed waiting for the semaphore.
        (assert (= (* 2 count) (count-if-not #'null values)))
        ;; The updated semaphore count should be in [0,19] at all
        ;; times.
        (assert (every (lambda (value) (<= 0 value (1- (* 2 count))))
                       (remove nil values)))
        ;; (At least) one thread should decrease the count to 0.
        (assert (find 0 values))))))

(with-test (:name (wait-on-semaphore semaphore-notification :lp-1038034)
            :skipped-on (not :sb-thread))
  ;; Test robustness of semaphore acquisition and notification with
  ;; asynchronous thread termination...  Which we know is currently
  ;; fragile.
  (dotimes (run 180)
    (let ((sem (make-semaphore)))
      ;; In CRITICAL, WAIT-ON-SEMAPHORE and SLEEP can be interrupted
      ;; by TERMINATE-THREAD below. But the SIGNAL-SEMAPHORE cleanup
      ;; cannot be interrupted.
      (flet ((critical (sleep)
               (let ((note (make-semaphore-notification)))
                 (sb-sys:without-interrupts
                     (unwind-protect
                          (sb-sys:with-local-interrupts
                            (wait-on-semaphore sem :notification note)
                            (sleep sleep))
                       ;; Re-increment on exit if we decremented it.
                       (when (semaphore-notification-status note)
                         (signal-semaphore sem)))))))
        ;; Create /parallel/ threads trying to acquire and then signal
        ;; the semaphore. Try to asynchronously abort T2 just as T1 is
        ;; exiting.
        (destructuring-bind (t1 t2 t3)
            (loop for i from 1
               for sleep in '(0.01 0.02 0.02)
               collect (make-thread #'critical :arguments sleep
                                    :name (format nil "T~A" i)))
          (signal-semaphore sem)
          (sleep 0.01)
          (ignore-errors
            (terminate-thread t2))
          (flet ((safe-join-thread (thread &key timeout
                                                abort)
                   (assert timeout)
                   (multiple-value-bind (value problem)
                       (join-thread thread
                                    :timeout timeout
                                    :default :timeout)
                     (unless (and abort
                                  (eq problem :abort))
                       (when (eq value :timeout)
                         (assert (eq problem :timeout))
                         (error "Hang in (join-thread ~A) ?" thread))))))
            (safe-join-thread t1 :timeout 60)
            (safe-join-thread t2 :timeout 60 :abort t)
            (safe-join-thread t3 :timeout 60)))))
    (when (zerop (mod run 60))
      (fresh-line)
      (write-string "; "))
    (write-char #\.)
    (force-output)))
