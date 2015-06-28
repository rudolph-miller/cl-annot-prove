(in-package :cl-user)
(defpackage cl-annot-prove
  (:use :cl
        :annot.doc
        :annot.util
        :annot.helper
        :prove)
  (:export :*symbol-tests-list*
           :query-symbol-tests
           :test-form
           :test-before
           :test-after
           :test-around
           :symbol-tests-symbol
           :symbol-tests-tests
           :symbol-tests-before
           :symbol-tests-after
           :symbol-tests-around
           :test-document
           :test-document-got
           :test-document-expected
           :render-symbol-tests
           :tests))
(in-package :cl-annot-prove)

(syntax:use-syntax :annot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Globals

@doc
"List of #S(SYMBOL-TESTS ...)s."
(defvar *symbol-tests-list* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; structs

@doc
"Structure of test."
(defstruct test
  (form)
  (before)
  (after)
  (around))

@doc
"Structure of tests for symbol."
(defstruct (symbol-tests (:constructor %make-symbol-tests))
  (symbol)
  (tests)
  (before)
  (after)
  (around))

(defun make-symbol-tests (symbol tests &key before after around before-each after-each around-each)
  (check-type tests list)
  (let* ((tests (when tests
                  (if (typep (car tests) 'test)
                      tests
                      (mapcar #'(lambda (form)
                                  (make-test :form form
                                             :before before-each
                                             :after after-each
                                             :around around-each))
                              tests))))
         (symbol-tests (%make-symbol-tests :symbol symbol :tests tests :before before :after after :around around)))
    (setq *symbol-tests-list*
          (cons symbol-tests
                (remove symbol-tests *symbol-tests-list*
                        :test #'(lambda (obj1 obj2)
                                  (eql (symbol-tests-symbol obj1)
                                       (symbol-tests-symbol obj2))))))
    symbol-tests))

(defstruct test-document
  (got)
  (expected))

(defmethod print-object ((object test-document) stream)
  (with-slots (got expected) object
    (format stream "~s~%;; => ~a" got expected)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; query

@doc
"Return list of #S(SYMBOL-TESTS ...)s which match the conditions."
(defun query-symbol-tests (&key symbol symbol-name symbol-package (symbol-tests-list *symbol-tests-list*))
  (labels ((equal-symbol-name (symbol symbol-name)
             (equal (symbol-name symbol) symbol-name))
           (equal-symbol-package-name (symbol package-name)
             (let ((symbol-package (symbol-package symbol)))
               (or (equal (package-name symbol-package) package-name)
                   (member package-name (package-nicknames symbol-package) :test #'equal)))))
    (loop with symbol-package-name = (when symbol-package
                                       (package-name (if (typep symbol-package 'package)
                                                         symbol-package
                                                         (find-package symbol-package))))
          for tests in symbol-tests-list
          for tests-symbol = (symbol-tests-symbol tests)
          when (cond
                 (symbol
                  (eql tests-symbol symbol))
                 ((and symbol-name symbol-package)
                  (and (equal-symbol-name tests-symbol symbol-name)
                       (equal-symbol-package-name tests-symbol symbol-package-name)))
                  (symbol-name
                   (equal-symbol-name tests-symbol symbol-name))
                  (symbol-package
                   (equal-symbol-package-name tests-symbol symbol-package-name))
                  (t nil))
                 collecting tests)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; render

(defun extract-test-document (form)
  (when (listp form)
    (let ((expected (extract-test-expected (car form) form)))
      (when expected
    (make-test-document :got (cadr form)
                        :expected expected)))))

(defgeneric extract-test-expected (operator form)
  (:method (operator form)
    (declare (ignore operator form))
    nil))

(defmacro def-extract-test-expected ((symbol form) &body body)
  (let ((operator (gensym "operator")))
    `(defmethod extract-test-expected ((,operator (eql ',symbol)) ,form)
       (declare (ignore ,operator))
       ,@body)))

(def-extract-test-expected (prove:ok form)
  "T")

(def-extract-test-expected (prove:is form)
  (format nil "~s" (caddr form)))

(def-extract-test-expected (prove:isnt form)
  (format nil "~s" (caddr form)))

(def-extract-test-expected (prove:is-values form)
  (format nil "(VALUES ~{~s~^ ~})" (caddr form)))

(def-extract-test-expected (prove:is-print form)
  (format nil "PRINT ~a" (caddr form)))

(def-extract-test-expected (prove:is-error form)
  (format nil "RAISE ~a" (caddr form)))

(def-extract-test-expected (prove:is-type form)
  (format nil "TYPE: ~a" (caddr form)))

(def-extract-test-expected (prove:like form)
  (format nil "LIKE: ~s" (caddr form)))

(def-extract-test-expected (prove:is-expand form)
  (format nil "EXPANDED TO: ~s" (caddr form)))

(defun replace-test-form (test-form)
  (or (extract-test-document test-form)
      (if (listp test-form)
          (mapcar #'(lambda (item)
                      (let ((result (replace-test-form item)))
                        (if (typep result 'test-document)
                            result
                            item)))
                  test-form)
          test-form)))

(defun replace-call-next-method (form new)
  (if (listp form)
      (if (equal form '(cl:call-next-method))
          new
          (mapcar #'(lambda (item)
                      (replace-call-next-method item new))
                  form))
      form))
  
(defun render-method-chain (main &key before after around)
  (let ((inner (if (or before after)
                   `(progn ,@(when before (list before))
                           ,main
                           ,@(when after (list after)))
                   main)))
    (if around
        (replace-call-next-method around inner)
        inner)))

(defun render-symbol-tests (symbol-tests)
  (labels ((render-around (test)
           (render-method-chain (or (test-around test) '(cl:call-next-method))
                                :before (symbol-tests-before symbol-tests)
                                :after (symbol-tests-after symbol-tests) :around (symbol-tests-around symbol-tests)))
           (render-test (test)
             (render-method-chain (replace-test-form (test-form test))
                                :around (render-around test))))
    (mapcar #'(lambda (test)
                (princ-to-string (render-test test)))
            (symbol-tests-tests symbol-tests))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; annotation

(defannotation tests (test-forms definition-form)
    (:arity 2)
  (progn-form-replace-last
   (lambda (definition-form)
     (case (definition-form-type definition-form)
       ((defun defmacro)
        (let ((symbol (cadr definition-form)))
          (make-symbol-tests symbol test-forms))
        definition-form)
       (otherwise (error "Test not supported: ~a" definition-form))))
   definition-form))

#|
Tasks:
- tests.before
- tests.after
- tests.around
- tests.before-each
- tests.after-each
- tests.around-each
|#
