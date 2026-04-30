(use-package "SB-THREAD")

;;; Terminating a thread that's waiting for the terminal.

(with-test (:name (:terminate-thread :get-foreground)
                  :skipped-on (not :sb-thread))
 (let ((thread (make-thread (lambda ()
                              (sb-thread:get-foreground)))))
   (sleep 1)
   (assert (thread-alive-p thread))
   (terminate-thread thread)
   (sleep 1)
   (assert (not (thread-alive-p thread)))))

;;; Condition-wait should not be interruptible under WITHOUT-INTERRUPTS
;;; BUT: Such a claim is without much merit. Even if a wait is not "interrupted",
;;; the very definition of spurious wakeup is that return from the wait happens
;;; for ANY reason - users of condition variables must ALWAYS anticipate needing
;;; to loop over a condition-wait.

(with-test (:name :without-interrupts+condition-wait
            :skipped-on (not :sb-thread))
  (let* ((lock (make-mutex))
         (queue (make-waitqueue))
         (actually-wakeup nil)
         (thread (make-thread (lambda ()
                                (sb-sys:without-interrupts
                                  (with-mutex (lock)
                                    (loop
                                     (condition-wait queue lock)
                                     (if actually-wakeup (return)))))))))
    (sleep .25)
    (assert (thread-alive-p thread))
    ;; this is the supposed "interrupt that doesn't interrupt",
    ;; but it _is_ permitted to wake the condition variable.
    (terminate-thread thread)
    (sleep .5)
    (assert (thread-alive-p thread))
    (setq actually-wakeup t)
    (sb-thread:barrier (:write))
    (condition-notify queue)
    (sleep .25)
    (assert (not (thread-alive-p thread)))))

;;; GRAB-MUTEX should not be interruptible under WITHOUT-INTERRUPTS

(with-test (:name :without-interrupts+grab-mutex
            :skipped-on (not :sb-thread))
  (let* ((lock (make-mutex))
         (bar (progn (grab-mutex lock) nil))
         (thread (make-thread (lambda ()
                                (sb-sys:without-interrupts
                                    (with-mutex (lock)
                                      (setf bar t)))))))
    (sleep 1)
    (assert (thread-alive-p thread))
    (terminate-thread thread)
    (sleep 1)
    (assert (thread-alive-p thread))
    (release-mutex lock)
    (sleep 1)
    (assert (not (thread-alive-p thread)))
    (assert (eq :aborted (join-thread thread :default :aborted)))
    (assert bar)))

(with-test (:name (:wait-for :deadline))
  (assert (eq :ok
              (sb-sys:with-deadline (:seconds 10)
                (assert (not (sb-ext:wait-for nil :timeout 0.1)))
                :ok)))
  (assert (eq :deadline
              (handler-case
                  (sb-sys:with-deadline (:seconds 0.1)
                    (sb-ext:wait-for nil :timeout 10)
                    (error "oops"))
                (sb-sys:deadline-timeout () :deadline)))))
