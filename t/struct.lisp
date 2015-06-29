(in-package :cl-user)
(defpackage cl-annot-prove-test.struct
  (:use :cl
        :cl-annot-prove
        :prove)
  (:import-from :cl-annot-prove.struct
                :make-test
                :make-symbol-tests
                :make-test-document))
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
  (let ((symbol-tests (make-symbol-tests 'sample '((is a b))
                                         :before '(print "before")
                                         :after '(print "after")
                                         :around '(let ((a 1)) (call-next-method))
                                         :before-each '(print "before-each")
                                         :after-each '(print "after-each")
                                         :around-each'(let ((b 1)) (call-next-method))))
        (*default-test-function* #'equal))
    (is-type symbol-tests
             'symbol-tests
             "can make-symbol-tests.")

    (is (symbol-tests-symbol symbol-tests)
        'sample
        "can bind symbol.")

    (is (symbol-tests-before symbol-tests)
        '(print "before")
        "can bind before.")

    (is (symbol-tests-after symbol-tests)
        '(print "after")
        "can bind after.")

    (is (symbol-tests-around symbol-tests)
        '(let ((a 1)) (call-next-method))
        "can bind around.")

    (let ((test (car (symbol-tests-tests symbol-tests))))
      (is-type test
               'test
               "can bind tests.")

      (is (test-form test)
          '(is a b)
          "can bind form of test.")

      (is (test-before test)
          '(print "before-each")
          "can bind before of test.")

      (is (test-after test)
          '(print "after-each")
          "can bind after of test.")

      (is (test-around test)
          '(let ((b 1)) (call-next-method))
          "can bind around of test."))))

(subtest "test-document"
  (let ((test-document (make-test-document :got '(is a 1) :expected 1)))
    (is-type test-document
             'test-document
             "can make-test-document.")

    (is (test-document-got test-document)
        '(is a 1)
        "can bind got.")

    (is (test-document-expected test-document)
        1
        "can bind expected.")

    (is (princ-to-string test-document)
        (format nil "(IS A 1)~%;; => 1")
        "can print test-document correctly.")))

(finalize)
