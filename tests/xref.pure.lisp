;;;; consistency tests with no side-effects of the xref en/decoder

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(with-test (:name (:xref :smoke :form-number))
  (let* ((data '(:sets ((x . 10) (x . 11) (y . 12))))
         (packed (sb-c::pack-xref-data data))
         (expected '((:sets x 10) (:sets x 11) (:sets y 12)))
         (map-result (make-hash-table :test 'equal)))
    (flet ((mapper (kind name form-number)
             (setf (gethash (list kind name form-number) map-result) t)))
      (sb-c:map-packed-xref-data #'mapper packed)
      (dolist (e expected)
        (assert (gethash e map-result)))
      (assert (= (hash-table-count map-result) 3)))))
