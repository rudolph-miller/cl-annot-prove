(in-package :cl-user)
(defpackage cl-annot-prove-test.struct
  (:use :cl
        :cl-annot-prove
        :prove)
  (:import-from :cl-annot-prove.struct
                :make-test))
(in-package :cl-annot-prove-test.struct)

(plan nil)

(subtest "test"
  (let ((test (make-test :form '(is a 1)
                         :before '(print "before")
                         :after '(print "after")
                         :around '(let ((a 1)) (call-next-method))))
        (*default-test-function* #'equal))
    (is-type test
             'test
             "can make-test.")

    (is (test-form test)
        '(is a 1)
        "can bind form.")

    (is (test-before test)
        '(print "before")
        "can bind before.")

    (is (test-after test)
        '(print "after")
        "can bind after.")

    (is (test-around test)
        '(let ((a 1)) (call-next-method))
        "can bind around.")))

(subtest "symbol-tests"
  (skip 1 "Not written yet."))

(subtest "test-document"
  (skip 1 "Not written yet."))

(finalize)
