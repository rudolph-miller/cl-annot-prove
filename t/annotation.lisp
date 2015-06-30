(in-package :cl-user)
(defpackage cl-annot-prove-test.annotation
  (:use :cl
        :cl-annot-prove
        :prove))
(in-package :cl-annot-prove-test.annotation)

(plan nil)

(syntax:use-syntax :annot)

@tests
((is (add 1 2) 3))
(defun add (a b)
  (+ a b))

(subtest "@tests"
  (is (length (query-symbol-tests :symbol 'add))
      1
      "can add tests."))

(finalize)
