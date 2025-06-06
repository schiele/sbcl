;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

;;;; copyright information from original PCL sources:
;;;;
;;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;;; All rights reserved.
;;;;
;;;; Use and copying of this software and preparation of derivative works based
;;;; upon this software are permitted. Any distribution of this software or
;;;; derivative works must comply with all applicable United States export
;;;; control laws.
;;;;
;;;; This software is made available AS IS, and Xerox Corporation makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.

(in-package "SB-PCL")

(defun get-method-function (method &optional method-alist wrappers)
  (let ((fn (cadr (assoc method method-alist))))
    (if fn
        (values fn nil nil nil)
        (multiple-value-bind (mf fmf)
            (if (listp method)
                (early-method-function method)
                (values nil (safe-method-fast-function method)))
          (let* ((pv-table (and fmf (method-plist-value method :pv-table))))
            (if (and fmf (or (null pv-table) wrappers))
                (let* ((pv-wrappers (when pv-table
                                      (pv-wrappers-from-all-wrappers
                                       pv-table wrappers)))
                       (pv (when (and pv-table pv-wrappers)
                             (pv-table-lookup pv-table pv-wrappers))))
                  (values mf t fmf pv))
                (values
                 (or mf (if (listp method)
                            (bug "early method with no method-function")
                            (method-function method)))
                 t nil nil)))))))

(defun make-effective-method-function (generic-function form &optional
                                       method-alist wrappers)
  (funcall (make-effective-method-function1 generic-function form
                                            (not (null method-alist))
                                            (not (null wrappers)))
           method-alist wrappers))

(defun make-effective-method-function1 (generic-function form
                                        method-alist-p wrappers-p)
  (if (and (listp form)
           (eq (car form) 'call-method)
           (not (gf-requires-emf-keyword-checks generic-function)))
      (make-effective-method-function-simple generic-function form)
      ;; We have some sort of `real' effective method. Go off and get a
      ;; compiled function for it. Most of the real hair here is done by
      ;; the GET-FUN mechanism.
      (make-effective-method-function-internal generic-function form
                                               method-alist-p wrappers-p)))

(defun make-effective-method-fun-type (generic-function
                                       form
                                       method-alist-p
                                       wrappers-p)
  (if (and (listp form)
           (eq (car form) 'call-method))
      (let* ((cm-args (cdr form))
             (method (car cm-args)))
        (when method
          (if (if (listp method)
                  (eq (car method) :early-method)
                  (method-p method))
              (if method-alist-p
                  t
                  (multiple-value-bind (mf fmf)
                      (if (listp method)
                          (early-method-function method)
                          (values nil (safe-method-fast-function method)))
                    (declare (ignore mf))
                    (let* ((pv-table (and fmf (method-plist-value method :pv-table))))
                      (if (and fmf (or (null pv-table) wrappers-p))
                          'fast-method-call
                          'method-call))))
              (if (and (consp method) (eq (car method) 'make-method))
                  (make-effective-method-fun-type
                   generic-function (cadr method) method-alist-p wrappers-p)
                  (type-of method)))))
      'fast-method-call))

(defun make-effective-method-function-simple
    (generic-function form &optional no-fmf-p)
  ;; The effective method is just a call to CALL-METHOD. This opens up
  ;; the possibility of just using the method function of the method as
  ;; the effective method function.
  ;;
  ;; But we have to be careful. If that method function will ask for
  ;; the next methods we have to provide them. We do not look to see
  ;; if there are next methods, we look at whether the method function
  ;; asks about them. If it does, we must tell it whether there are
  ;; or aren't to prevent the leaky next methods bug.
  (let* ((cm-args (cdr form))
         (fmf-p (and (null no-fmf-p)
                     (or (not (eq **boot-state** 'complete))
                         (gf-fast-method-function-p generic-function))
                     (null (cddr cm-args))))
         (method (car cm-args))
         (cm-args1 (cdr cm-args)))
    (lambda (method-alist wrappers)
      (make-effective-method-function-simple1 generic-function
                                              method
                                              cm-args1
                                              fmf-p
                                              method-alist
                                              wrappers))))

;;; methods-tracing TODO:
;;;
;;; 2. tracing method calls for non-fast-method-function calls
;;;    - [DONE] the calls themselves
;;;    - calls to the METHOD-FUNCTION of methods with fast functions
;;;      (e.g. from something implementing CALL-NEXT-METHOD; handle this with
;;;      some more smarts in %METHOD-FUNCTION objects?)
;;;    - calls to the METHOD-FUNCTION of methods without fast functions
;;;      (TRACE :METHODS T /could/ modify the METHOD-FUNCTION slot)
;;; 4. tracing particular methods
;;;    - need an interface.
;;;      * (trace (method foo :around (t)))? [ how to trace the method and not
;;;        the generic function as a whole?]
;;;      * (trace :methods '((:around (t))) foo)? [probably not, interacts
;;;        poorly with TRACE arg handling]
;;; 5. supporting non-munged arguments as an option

(defun method-trace-name (gf method)
  ;; KLUDGE: we abuse NIL as second argument to mean that this is a
  ;; combined method (i.e. something resulting from MAKE-METHOD in a
  ;; method combination, rather than CALL-METHOD on a method object).
  (if method
      `(method ,(generic-function-name gf)
               ,@(method-qualifiers method)
               ,(unparse-specializers gf (method-specializers method)))
      `(combined-method ,(generic-function-name gf))))

(defun maybe-trace-method (gf method fun fmf-p)
  (let ((m-name (when (plusp (hash-table-count sb-debug::*traced-funs*))
                  ;; KLUDGE: testing if *TRACE-FUNS* has anything anything to
                  ;; avoid calling METHOD-TRACE-NAME during PCL bootstrapping
                  ;; when the generic-function type is not yet defined.)
                  (method-trace-name gf method))))
    (when m-name
      (sb-debug::retrace-local-funs m-name))
    (let ((info (when m-name
                  (or (gethash m-name sb-debug::*traced-funs*)
                      (let ((gf-info (gethash (or (generic-function-name gf) gf)
                                              sb-debug::*traced-funs*)))
                        (when (and gf-info (sb-debug::trace-info-methods gf-info))
                          (let ((copy (copy-structure gf-info)))
                            (setf (sb-debug::trace-info-what copy) m-name)
                            copy)))))))
      (if info
          (lambda (&rest args)
            (apply #'sb-debug::trace-method-call info fun fmf-p args))
          fun))))

(defun make-emf-from-method
    (gf method cm-args fmf-p &optional method-alist wrappers)
  ;; Avoid style-warning about compiler-macro being unavailable.
  (declare (notinline make-instance))
  (multiple-value-bind (mf real-mf-p fmf pv)
      (get-method-function method method-alist wrappers)
    (if fmf
        (let* ((next-methods (car cm-args))
               (next (make-effective-method-function-simple1
                      gf (car next-methods)
                      (list* (cdr next-methods) (cdr cm-args))
                      fmf-p method-alist wrappers))
               (arg-info (method-plist-value method :arg-info))
               (default (cons nil nil))
               (value (method-plist-value method :constant-value default))
               (fun (maybe-trace-method gf method fmf t)))
          (if (eq value default)
              (make-fast-method-call
               :function fun :pv pv :next-method-call next :arg-info arg-info)
              (make-constant-fast-method-call
               :function fun :pv pv :next-method-call next
               :arg-info arg-info :value value)))
        (if real-mf-p
            (flet ((frob-cm-arg (arg)
                     (if (if (listp arg)
                             (eq (car arg) :early-method)
                             (method-p arg))
                         arg
                         (if (and (consp arg) (eq (car arg) 'make-method))
                             (let ((emf (make-effective-method-function
                                         gf (cadr arg) method-alist wrappers)))
                               (etypecase emf
                                 (method-call
                                  (make-instance 'standard-method
                                                 :specializers nil ; XXX
                                                 :qualifiers nil ; XXX
                                                 :function (method-call-function emf)))
                                 (fast-method-call
                                  (let* ((fmf (fast-method-call-function emf))
                                         (fun (method-function-from-fast-method-call emf))
                                         (mf (%make-method-function fmf)))
                                    (setf (%funcallable-instance-fun mf) fun)
                                    (make-instance 'standard-method
                                                   :specializers nil ; XXX
                                                   :qualifiers nil
                                                   :function mf)))))
                             arg))))
              (let* ((default (cons nil nil))
                     (value
                      (method-plist-value method :constant-value default))
                     ;; FIXME: this is wrong.  Very wrong.  It assumes
                     ;; that the only place that can have make-method
                     ;; calls is in the list structure of the second
                     ;; argument to CALL-METHOD, but AMOP says that
                     ;; CALL-METHOD can be more complicated if
                     ;; COMPUTE-EFFECTIVE-METHOD (and presumably
                     ;; MAKE-METHOD-LAMBDA) is adjusted to match.
                     ;;
                     ;; On the other hand, it's a start, because
                     ;; without this calls to MAKE-METHOD in method
                     ;; combination where one of the methods is of a
                     ;; user-defined class don't work at all.  -- CSR,
                     ;; 2006-08-05
                     (args (cons (mapcar #'frob-cm-arg (car cm-args))
                                 (cdr cm-args)))
                     (fun (maybe-trace-method gf method mf nil)))
                (if (eq value default)
                    (make-method-call :function fun :call-method-args args)
                    (make-constant-method-call
                     :function fun :value value :call-method-args args))))
            mf))))

(defun make-effective-method-function-simple1
    (gf method cm-args fmf-p &optional method-alist wrappers)
  (when method
    (if (if (listp method)
            (eq (car method) :early-method)
            (method-p method))
        (make-emf-from-method gf method cm-args fmf-p method-alist wrappers)
        (if (and (consp method) (eq (car method) 'make-method))
            (make-effective-method-function gf
                                            (cadr method)
                                            method-alist wrappers)
            method))))

(defvar *global-effective-method-gensyms* ())
(defvar *rebound-effective-method-gensyms*)

(defun get-effective-method-gensym ()
  (or (pop *rebound-effective-method-gensyms*)
      (let ((new (pcl-symbolicate "EFFECTIVE-METHOD-GENSYM-"
                                    (length *global-effective-method-gensyms*))))
        (setq *global-effective-method-gensyms*
              (append *global-effective-method-gensyms* (list new)))
        new)))

(let ((*rebound-effective-method-gensyms* ()))
  (dotimes-fixnum (i 10) (get-effective-method-gensym)))

(defun expand-effective-method-function (gf effective-method &optional env)
  (declare (ignore env))
  (declare (muffle-conditions code-deletion-note))
  (multiple-value-bind (nreq applyp)
      (get-generic-fun-info gf)
    (let ((ll (make-fast-method-call-lambda-list nreq applyp))
          (mc-args-p
           (when (eq **boot-state** 'complete)
             ;; Otherwise the METHOD-COMBINATION slot is not bound.
             (let ((combin (generic-function-method-combination gf)))
               (and (long-method-combination-p combin)
                    (long-method-combination-args-lambda-list combin)))))
          ;; FIXME: this name in the lambda almost completely defeats
          ;; the fngen cache when compiling method combinations
          ;; since no two expressions will be EQUAL. Perhaps we should
          ;; teach fngen to remove the name?
          (name `(emf ,(generic-function-name gf))))
      (cond
        (mc-args-p
         (let* ((required (make-dfun-required-args nreq))
                (gf-args (if applyp
                             `(list* ,@required (sb-c::%rest-list .rest.))
                             `(list ,@required))))
           `(named-lambda ,name ,ll
              (declare (ignore .pv. .next-method-call.))
              (let ((.gf-args. ,gf-args))
                (declare (ignorable .gf-args.))
                ,effective-method))))
        (t
         `(named-lambda ,name ,ll
            (declare (ignore .pv. .next-method-call.))
            (declare (ignorable ,@(make-dfun-required-args nreq)
                                ,@(when applyp '(.rest.))))
            ,effective-method))))))

(defun expand-emf-call-method (gf form metatypes applyp env)
  (declare (ignore gf metatypes applyp env))
  `(call-method ,(cdr form)))

(defmacro call-method (&rest args)
  (declare (ignore args))
  ;; the PROGN is here to defend against premature macroexpansion by
  ;; RESTART-CASE.
  `(progn (error "~S outside of a effective method form" 'call-method)))

(defun make-effective-method-list-fun-type
    (generic-function form method-alist-p wrappers-p)
  (if (every (lambda (form)
               (eq 'fast-method-call
                   (make-effective-method-fun-type
                    generic-function form method-alist-p wrappers-p)))
             (cdr form))
      'fast-method-call
      t))

(defun memf-test-converter (form generic-function method-alist-p wrappers-p)
  (case (and (consp form) (car form))
    (call-method
     (case (make-effective-method-fun-type
            generic-function form method-alist-p wrappers-p)
       (fast-method-call '.fast-call-method.)
       (t '.call-method.)))
    (call-method-list
     (case (make-effective-method-list-fun-type
            generic-function form method-alist-p wrappers-p)
       (fast-method-call '.fast-call-method-list.)
       (t '.call-method-list.)))
    (t (default-test-converter form))))

;;; CMUCL comment (2003-10-15):
;;;
;;;   This function is called via the GET-FUNCTION mechanism on forms
;;;   of an emf lambda.  First value returned replaces FORM in the emf
;;;   lambda.  Second value is a list of variable names that become
;;;   closure variables.
(defun memf-code-converter
    (form generic-function metatypes applyp method-alist-p wrappers-p)
  (case (and (consp form) (car form))
    (call-method
     (let ((gensym (get-effective-method-gensym)))
       (values (make-emf-call
                (length metatypes) applyp gensym
                (make-effective-method-fun-type
                 generic-function form method-alist-p wrappers-p))
               (list gensym))))
    (call-method-list
     (let ((gensym (get-effective-method-gensym))
           (type (make-effective-method-list-fun-type
                  generic-function form method-alist-p wrappers-p)))
       (values `(dolist (emf ,gensym nil)
                 ,(make-emf-call (length metatypes) applyp 'emf type))
               (list gensym))))
    (t
     (default-code-converter form))))

(defun memf-constant-converter (form generic-function)
  (case (and (consp form) (car form))
    (call-method
     (list (cons '.meth.
                 (make-effective-method-function-simple
                  generic-function form))))
    (call-method-list
     (list (cons '.meth-list.
                 (mapcar (lambda (form)
                           (make-effective-method-function-simple
                            generic-function form))
                         (cdr form)))))
    (t
     (default-constant-converter form))))

(defun make-effective-method-function-internal
    (generic-function effective-method method-alist-p wrappers-p)
  (multiple-value-bind (nreq applyp metatypes nkeys arg-info)
      (get-generic-fun-info generic-function)
    (declare (ignore nkeys arg-info))
    (let* ((*rebound-effective-method-gensyms*
            *global-effective-method-gensyms*)
           (name (if (early-gf-p generic-function)
                     (!early-gf-name generic-function)
                     (generic-function-name generic-function)))
           (arg-info (cons nreq applyp))
           (effective-method-lambda (expand-effective-method-function
                                     generic-function effective-method)))
      (multiple-value-bind (cfunction constants)
          (get-fun effective-method-lambda
                   (lambda (form)
                     (memf-test-converter form generic-function
                                          method-alist-p wrappers-p))
                   (lambda (form)
                     (memf-code-converter form generic-function
                                          metatypes applyp
                                          method-alist-p wrappers-p))
                   (lambda (form)
                     (memf-constant-converter form generic-function)))
        (lambda (method-alist wrappers)
          (flet ((compute-constant (constant)
                   (if (consp constant)
                       (case (car constant)
                         (.meth.
                          (funcall (cdr constant) method-alist wrappers))
                         (.meth-list.
                          (mapcar (lambda (fn)
                                    (funcall fn method-alist wrappers))
                                  (cdr constant)))
                         (t constant))
                       (case constant
                         (t constant)))))
            (let ((fun (apply cfunction
                              (mapcar #'compute-constant constants))))
              (set-fun-name fun `(combined-method ,name))
              (make-fast-method-call :function (maybe-trace-method generic-function nil fun t)
                                     :arg-info arg-info))))))))

(defmacro call-method-list (&rest calls)
  `(progn ,@calls))

(defun make-call-methods (methods)
  `(call-method-list
    ,@(mapcar (lambda (method) `(call-method ,method ())) methods)))

(defun gf-requires-emf-keyword-checks (generic-function)
  (member '&key (gf-lambda-list generic-function)))

(defconstant-eqx +standard-method-combination-qualifiers+
    '(:around :before :after) #'equal)

(defun standard-method-combination-qualifier-p (qualifier)
  (member qualifier +standard-method-combination-qualifiers+))

(defun standard-compute-effective-method
    (generic-function combin applicable-methods)
  (collect ((before) (primary) (after) (around))
    (flet ((invalid (method)
             (return-from standard-compute-effective-method
               `(invalid-qualifiers ',generic-function ',combin ',method))))
      (dolist (m applicable-methods)
        (let ((qualifiers (if (listp m)
                              (early-method-qualifiers m)
                              (safe-method-qualifiers m))))
          (cond
            ((null qualifiers) (primary m))
            ((cdr qualifiers) (invalid m))
            ((eq (car qualifiers) :around) (around m))
            ((eq (car qualifiers) :before) (before m))
            ((eq (car qualifiers) :after) (after m))
            (t (invalid m))))))
    (cond ((null applicable-methods)
           ;; APPLICABLE-METHODS is normally non-null in effective
           ;; method computation, but COMPUTE-APPLICABLE-METHODS can
           ;; in principle be called by MetaObject Protocol programmers.
           `(method-combination-error
             "No applicable method found for ~S"
             ',generic-function))
          ((null (primary))
           ;; PCL checks for no primary method before method
           ;; combination, but a MetaObject Protocol programmer could
           ;; call COMPUTE-EFFECTIVE-METHOD themselves and end up
           ;; here.
           `(method-combination-error
             "No primary method found for ~S among applicable methods: ~S"
             ',generic-function (list ,@(mapcar (lambda (m) `(quote ,m)) applicable-methods))))
          ((and (null (before)) (null (after)) (null (around)))
           ;; By returning a single call-method `form' here we enable
           ;; an important implementation-specific optimization; that
           ;; is, we can use the fast method function directly as the
           ;; effective method function.
           ;;
           ;; However, the requirement by ANSI (CLHS 7.6.5) on generic
           ;; function argument checking inhibits this, as we don't
           ;; perform this checking in fast-method-functions given
           ;; that they are not solely used for effective method
           ;; functions, but also in combination, when they should not
           ;; perform argument checks.  We still return the bare
           ;; CALL-METHOD, but the caller is responsible for ensuring
           ;; that keyword applicability is checked if this is a fast
           ;; method function used in an effective method.  (See
           ;; WRAP-WITH-APPLICABLE-KEYWORD-CHECK below).
           `(call-method ,(first (primary)) ,(rest (primary))))
          (t
           (let ((main-effective-method
                   (if (or (before) (after))
                       `(multiple-value-prog1
                          (progn
                            ,(make-call-methods (before))
                            (call-method ,(first (primary))
                                         ,(rest (primary))))
                          ,(make-call-methods (reverse (after))))
                       `(call-method ,(first (primary)) ,(rest (primary))))))
             (if (around)
                 `(call-method ,(first (around))
                               (,@(rest (around))
                                  (make-method ,main-effective-method)))
                 main-effective-method))))))

(defun short-method-combination-qualifiers (type-name)
  (list type-name :around))

(defun short-method-combination-qualifier-p (type-name qualifier)
  (or (eq qualifier type-name) (eq qualifier :around)))

(defun short-compute-effective-method
    (generic-function combin applicable-methods)
  (let ((type-name (method-combination-type-name combin))
        (operator (short-combination-operator combin))
        (ioa (short-combination-identity-with-one-argument combin))
        (order (car (method-combination-options combin)))
        (around ())
        (primary ()))
    (flet ((invalid (method)
             (return-from short-compute-effective-method
               `(invalid-qualifiers ',generic-function ',combin ',method))))
      (dolist (m applicable-methods)
        (let ((qualifiers (method-qualifiers m)))
          (cond ((null qualifiers) (invalid m))
                ((cdr qualifiers) (invalid m))
                ((eq (car qualifiers) :around)
                 (push m around))
                ((eq (car qualifiers) type-name)
                 (push m primary))
                (t (invalid m))))))
    (setq around (nreverse around))
    (ecase order
      (:most-specific-last) ; nothing to be done, already in correct order
      (:most-specific-first
       (setq primary (nreverse primary))))
    (let ((main-method
            (if (and (null (cdr primary))
                     (not (null ioa)))
                `(call-method ,(car primary) ())
                `(,operator ,@(mapcar (lambda (m) `(call-method ,m ()))
                                      primary)))))
      (cond ((null applicable-methods)
             ;; APPLICABLE-METHODS is normally non-null in effective
             ;; method computation, but COMPUTE-APPLICABLE-METHODS can
             ;; in principle be called by MetaObject Protocol programmers.
             `(method-combination-error
               "No applicable method found for ~S"
               ',generic-function))
            ((null primary)
             ;; PCL checks for no primary method before method
             ;; combination, but a MetaObject Protocol programmer could
             ;; call COMPUTE-EFFECTIVE-METHOD themselves and end up
             ;; here.
             `(method-combination-error
               "No primary method found for ~S among applicable methods: ~S"
               ',generic-function (list ,@(mapcar (lambda (m) `(quote ,m)) applicable-methods))))
            ((null around) main-method)
            (t
             `(call-method ,(car around)
                           (,@(cdr around) (make-method ,main-method))))))))

;;; helper code for checking keywords in generic function calls.
(defun compute-applicable-keywords (gf methods)
  (let ((any-keyp nil))
    (flet ((analyze (lambda-list)
             (multiple-value-bind (llks nreq nopt keys)
                 (analyze-lambda-list lambda-list)
               (declare (ignore nreq))
               (when (ll-kwds-keyp llks)
                 (setq any-keyp t))
               (values nopt (ll-kwds-allowp llks) keys))))
      (multiple-value-bind (nopt allowp keys)
          (analyze (gf-lambda-list gf))
        (dolist (method methods)
          (let ((ll (if (consp method)
                        (early-method-lambda-list method)
                        (method-lambda-list method))))
            (multiple-value-bind (n allowp method-keys)
                (analyze ll)
              (declare (ignore n))
              (when allowp
                (return-from compute-applicable-keywords (values t nopt)))
              (setq keys (union method-keys keys)))))
        (aver any-keyp)
        (values (if allowp t keys) nopt)))))

(defun check-applicable-keywords (start valid-keys more-context more-count)
  (let ((allow-other-keys-seen nil)
        (allow-other-keys nil)
        (i start))
    (declare (type index i more-count)
             (optimize speed))
    (flet ((current-value ()
             (sb-c::%more-arg more-context i)))
      (declare (inline current-value))
      (collect ((invalid))
        (loop
           (when (>= i more-count)
             (when (and (invalid) (not allow-other-keys))
               (%program-error "~@<invalid keyword argument~P: ~
                                ~{~S~^, ~} (valid keys are ~{~S~^, ~}).~@:>"
                               (length (invalid)) (invalid) valid-keys))
             (return))
           (let ((key (current-value)))
             (incf i)
             (cond
               ((not (symbolp key))
                (%program-error "~@<keyword argument not a symbol: ~S.~@:>"
                                key))
               ((= i more-count)
                (sb-c::%odd-key-args-error))
               ((eq key :allow-other-keys)
                ;; only the leftmost :ALLOW-OTHER-KEYS has any effect
                (unless allow-other-keys-seen
                  (setq allow-other-keys-seen t
                        allow-other-keys (current-value))))
               ((eq t valid-keys))
               ((not (memq key valid-keys)) (invalid key))))
           (incf i))))))

(defun wrap-with-applicable-keyword-check (effective valid-keys keyargs-start)
  `(let ((.valid-keys. ',valid-keys)
         (.keyargs-start. ',keyargs-start))
     (multiple-value-bind (.more-context. .more-count.) (sb-c::%rest-context .rest.)
      (check-applicable-keywords
       .keyargs-start. .valid-keys. .more-context. .more-count.))
     ,effective))

;;;; the STANDARD method combination type. This is coded by hand
;;;; (rather than with DEFINE-METHOD-COMBINATION) for bootstrapping
;;;; and efficiency reasons. Note that the definition of the
;;;; FIND-METHOD-COMBINATION-METHOD appears in the file
;;;; defcombin.lisp. This is because EQL methods can't appear in the
;;;; bootstrap.
;;;;
;;;; The DEFCLASS for the METHOD-COMBINATION and
;;;; STANDARD-METHOD-COMBINATION classes has to appear here for this
;;;; reason. This code must conform to the code in the file
;;;; defcombin.lisp, look there for more details.

(defun compute-effective-method (generic-function combin applicable-methods)
  (standard-compute-effective-method generic-function
                                     combin
                                     applicable-methods))

;;; not INVALID-METHOD-ERROR as that would violate CLHS 11.1.2.1.1
(define-condition invalid-method-program-error (program-error simple-condition)
  ())
(defun invalid-method-error (method format-control &rest format-arguments)
  (let ((sb-debug:*stack-top-hint* (find-caller-frame)))
    (error 'invalid-method-program-error
           :format-control "~@<invalid method error for ~2I~_~S ~I~_method: ~2I~_~?~:>"
           :format-arguments (list method format-control format-arguments))))

;;; not METHOD-COMBINATION-ERROR as that would violate CLHS 11.1.2.1.1
(define-condition method-combination-program-error (program-error simple-condition)
  ())
(defun method-combination-error (format-control &rest format-arguments)
  (let ((sb-debug:*stack-top-hint* (find-caller-frame)))
    (error 'method-combination-program-error
           :format-control "~@<method combination error in CLOS dispatch: ~2I~_~?~:>"
           :format-arguments (list format-control format-arguments))))
