(in-package :cl-user)
(defpackage cl-annot-prove.struct
  (:use :cl
        :annot.doc)
  (:export :*symbol-tests-list*
           :test
           :test-form
           :test-before
           :test-after
           :test-around
           :symbol-tests
           :symbol-tests-symbol
           :symbol-tests-tests
           :symbol-tests-before
           :symbol-tests-after
           :symbol-tests-around
           :make-symbol-tests
           :test-document
           :test-document-got
           :test-documest-expected
           :make-test-document))
(in-package :cl-annot-prove.struct)

(syntax:use-syntax :annot)

@doc
"List of #S(SYMBOL-TESTS ...)s."
(defvar *symbol-tests-list* nil)

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
