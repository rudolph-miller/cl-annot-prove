(in-package :cl-user)
(defpackage cl-annot-prove.render
  (:use :cl
        :annot.doc
        :cl-annot-prove.struct)
  (:import-from :trivial-types
                :proper-list-p)
  (:export :call-tests
           :replace-call-tests
           :render-method-chain
           :render-around
           :expected-formatter
           :replace-test-with-setq-form
           :replace-test-form
           :render-symbol-tests))
(in-package :cl-annot-prove.render)

(syntax:use-syntax :annot)

(defun call-tests ())

(defmacro stub-progn (&body body)
  `(progn ,@body))

(defun replace-call-tests (form new)
  (if (listp form)
      (if (equal form '(call-tests))
          new
          (mapcar #'(lambda (item)
                      (replace-call-tests item new))
                  form))
      form))

(defun render-method-chain (main &key before after around)
  (let ((inner (if (or before after)
                   `(stub-progn ,@(when before (list before))
                      ,main
                      ,@(when after (list after)))
                   main)))
    (if around
        (replace-call-tests around inner)
        inner)))

(defun render-around (symbol-tests)
  (render-method-chain (or (symbol-tests-around-each symbol-tests) '(call-tests))
                       :before (symbol-tests-before symbol-tests)
                       :after (symbol-tests-after symbol-tests)
                       :around (symbol-tests-around symbol-tests)))

(defgeneric expected-formatter (operator)
  (:method (operator)
    (declare (ignore operator))
    nil))

(defmacro def-expected-formatter ((symbol expected) &body body)
  (let ((operator (gensym "operator")))
    `(defmethod expected-formatter ((,operator (eql ',symbol)))
       (declare (ignore ,operator))
       (lambda (,expected)
         ,@body))))

(def-expected-formatter (prove:ok expected)
  (declare (ignore expected))
  "T")

(def-expected-formatter (prove:is expected)
  (format nil "~s" expected))

(def-expected-formatter (prove:isnt expected)
  (format nil "~s" expected))

(def-expected-formatter (prove:is-values expected)
  (format nil "(VALUES ~{~s~^ ~})" expected))

(def-expected-formatter (prove:is-print expected)
  (format nil "PRINT ~a" expected))

(def-expected-formatter (prove:is-error expected)
  (format nil "RAISE ~a" expected))

(def-expected-formatter (prove:is-type expected)
  (format nil "TYPE: ~a" expected))

(def-expected-formatter (prove:like expected)
  (format nil "LIKE: ~a" expected))

(def-expected-formatter (prove:is-expand expected)
  (format nil "EXPANDED TO: ~s" expected))

(defun eval-silently (form)
  (let ((*standard-output* (make-broadcast-stream)))
    (handler-bind ((warning (lambda (c)
                              (declare (ignore c))
                              (muffle-warning))))
      (eval form))))

(defun replace-test-with-setq-form (form)
  (let (results)
    (labels ((set-result (form formatter)
               (let* ((result (gensym "result"))
                      (setq-form `(setq ,result ,(caddr form))))
                 (push (cons result (list :got (cadr form) :setq setq-form :formatter formatter)) results)
                 setq-form))
             (sub (form)
               (if (proper-list-p form)
                   (let* ((op (car form))
                          (formatter (expected-formatter op)))
                     (if formatter
                         (set-result form formatter)
                         (mapcar #'(lambda (item)
                                     (sub item))
                                 form)))
                   form)))
      (values (sub form) results))))

(defun replace-test-form (test-form around)
  (multiple-value-bind (replaced-form results) (replace-test-with-setq-form test-form)
    (let* ((result-symbols (mapcar #'car results))
           (result-setq-forms (mapcar #'(lambda (item) (getf (cdr item) :setq)) results))
           (result-around `(let (,@result-symbols) (call-tests) (list ,@result-symbols)))
           (result-values (eval-silently
                           (render-method-chain replaced-form
                                                :around (if around
                                                            (render-method-chain around
                                                                                 :around result-around)
                                                            result-around)))))
      (loop for i from 0
            for setq-form in result-setq-forms
            for result = (cdr (elt results i))
            for result-value = (elt result-values i)
            for got = (getf result :got)
            for formatter = (getf result :formatter)
            for expected = (funcall formatter result-value)
            do (setq replaced-form
                     (subst (make-test-document :got got
                                                :expected expected)
                            setq-form
                            replaced-form)))
      replaced-form)))

(defun replace-stub-progn (form)
  (let ((toplevel t))
    (labels ((sub (form)
               (if (proper-list-p form)
                   (if (and toplevel
                            (eql (car form) 'stub-progn)
                            (or (setq toplevel nil) t))
                       `(progn ,@(sub (cdr form)))
                       (mapcan #'(lambda (item)
                                   (if (and (proper-list-p item)
                                            (eql (car item) 'stub-progn))
                                       (sub (cdr item))
                                       (list (sub item))))
                               form))
                   form)))
      (sub form))))

@doc
"Render #S(SYMBOL-TESTS ...) for documents."
(defun render-symbol-tests (symbol-tests)
  (let* ((around (render-around symbol-tests))
         (replaced-tests (mapcar #'(lambda (test)
                                     (render-method-chain (replace-test-form test around)
                                                          :before (symbol-tests-before-each symbol-tests)
                                                          :after (symbol-tests-after-each symbol-tests)))
                                 (symbol-tests-tests symbol-tests))))
    (format nil "~s"
            (replace-stub-progn
             (render-method-chain (if (= (length replaced-tests) 1)
                                      (car replaced-tests)
                                      `(stub-progn ,@replaced-tests))
                                  :around around)))))
