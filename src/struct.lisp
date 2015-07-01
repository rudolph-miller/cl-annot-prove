(in-package :cl-user)
(defpackage cl-annot-prove.struct
  (:use :cl
        :annot.doc)
  (:export :*symbol-tests-list*
           :symbol-tests
           :symbol-tests-symbol
           :symbol-tests-tests
           :symbol-tests-before
           :symbol-tests-after
           :symbol-tests-around
           :symbol-tests-before-each
           :symbol-tests-after-each
           :symbol-tests-around-each
           :make-symbol-tests
           :add-symbol-tests
           :test-document
           :test-document-got
           :test-document-expected
           :make-test-document))
(in-package :cl-annot-prove.struct)

(syntax:use-syntax :annot)

@doc
"List of #S(SYMBOL-TESTS ...)s."
(defvar *symbol-tests-list* nil)

@doc
"Structure of tests for symbol."
(defstruct (symbol-tests (:constructor make-symbol-tests (symbol &key tests before after around before-each after-each around-each)))
  (symbol)
  (tests)
  (before)
  (after)
  (around)
  (before-each)
  (after-each)
  (around-each))

(defun add-symbol-tests (symbol-tests)
  (setq *symbol-tests-list*
        (cons symbol-tests
              (remove symbol-tests *symbol-tests-list*
                      :test #'(lambda (obj1 obj2)
                                (eql (symbol-tests-symbol obj1)
                                     (symbol-tests-symbol obj2)))))))

(defstruct test-document
  (got)
  (expected))

(defmethod print-object ((object test-document) stream)
  (with-slots (got expected) object
    (format stream "~s~%;; => ~a" got expected)))
