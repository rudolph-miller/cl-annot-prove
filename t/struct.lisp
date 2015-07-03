(in-package :cl-user)
(defpackage cl-annot-prove-test.struct
  (:use :cl
        :cl-annot-prove
        :prove)
  (:import-from :cl-annot-prove.struct
                :make-symbol-tests
                :add-symbol-tests
                :make-test-document))
(in-package :cl-annot-prove-test.struct)

(plan nil)

(subtest "symbol-tests"
  (let ((symbol-tests (make-symbol-tests 'sample
                                         :tests '((is a b))
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

    (is (car (symbol-tests-tests symbol-tests))
        '(is a b)
        "can bind tests.")

    (is (symbol-tests-before-each symbol-tests)
        '(print "before-each")
        "can bind before-each.")

    (is (symbol-tests-after-each symbol-tests)
        '(print "after-each")
        "can bind after-each.")

    (is (symbol-tests-around-each symbol-tests)
        '(let ((b 1)) (call-next-method))
        "can bind around-each.")))

(subtest "add-symbol-tests"
  (let* ((symbol (gensym))
         (symbol-tests (make-symbol-tests symbol
                                          :tests '((is 1 1)))))
    (is (query-symbol-tests :symbol symbol)
        nil
        "At first, symbol-tests is not yet registered.")

    (add-symbol-tests symbol-tests)

    (is (length (query-symbol-tests :symbol symbol))
        1
        "can register symbol-tests.")))

(defstruct some-struct
  (too-too-too-long-slot-name1)
  (too-long-slot-name2))

(subtest "test-document"
  (let ((test-document (make-test-document :got '(is a 1) :expected "1")))
    (is-type test-document
             'test-document
             "can make-test-document.")

    (is (test-document-got test-document)
        '(is a 1)
        "can bind got.")

    (is (test-document-expected test-document)
        "1"
        "can bind expected.")

    (is (princ-to-string test-document)
        (format nil "(IS A 1)~%;; => 1")
        "can print test-document correctly."))

  (let ((test-document
          (make-test-document :got '(is a b) :expected (format nil "~s"
                                                               (make-some-struct :too-too-too-long-slot-name1 "sample1"
                                                                                 :too-long-slot-name2 "sample2")))))
    (is (princ-to-string test-document)
        "(IS A B)
;; => #S(SOME-STRUCT
;;       :TOO-TOO-TOO-LONG-SLOT-NAME1 \"sample1\"
;;       :TOO-LONG-SLOT-NAME2 \"sample2\")"
        "can print multi-line.")))

(finalize)
