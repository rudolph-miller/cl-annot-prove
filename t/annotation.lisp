(in-package :cl-user)
(defpackage cl-annot-prove-test.annotation
  (:use :cl
        :cl-annot-prove
        :prove))
(in-package :cl-annot-prove-test.annotation)

(plan nil)

(syntax:use-syntax :annot)

@tests.around
(let ((a 1)) (call-next-method))
@tests.before
(print "before")
@tests.after
(print "after")
@tests.around.each
(let ((b 2)) (call-next-method))
@tests.before.each
(print "before-each")
@tests.after.each
(print "after-each")
@tests
((is (add a b) 3))
(defun add (a b)
  (+ a b))

(defun get-symbol-tests ()
  (car (query-symbol-tests :symbol 'add)))

(subtest "add-symbol-tests"
  (is-type (get-symbol-tests)
           'symbol-tests
           "can add symbol-tests."))

(defmacro with-symbol-tests (&body body)
  `(let ((symbol-tests (get-symbol-tests)))
     (declare (ignorable symbol-tests))
     ,@body))

(defmacro symbol-tests-subtest (desc &body body)
  `(subtest ,desc
     (let ((*default-test-function* #'equal))
       (with-symbol-tests
         ,@body))))

(symbol-tests-subtest "@tests"
  (is (symbol-tests-tests symbol-tests)
      '((is (add a b) 3))
      "can bind tests."))

(symbol-tests-subtest "@tests.before"
  (is (symbol-tests-before symbol-tests)
      '(print "before")
      "can bind before."))

(symbol-tests-subtest "@tests.after"
  (is (symbol-tests-after symbol-tests)
      '(print "after")
      "can bind after."))

(symbol-tests-subtest "@tests.around"
  (is (symbol-tests-around symbol-tests)
      '(let ((a 1)) (call-next-method))
      "can bind around."))

(symbol-tests-subtest "@tests.before.each"
  (is (symbol-tests-before-each symbol-tests)
      '(print "before-each")
      "can bind before-each."))

(symbol-tests-subtest "@tests.after.each"
  (is (symbol-tests-after-each symbol-tests)
      '(print "after-each")
      "can bind after-each."))

(symbol-tests-subtest "@tests.around.each"
  (is (symbol-tests-around-each symbol-tests)
      '(let ((b 2)) (call-next-method))
      "can bind around-each."))

(finalize)
