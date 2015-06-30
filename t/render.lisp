(in-package :cl-user)
(defpackage cl-annot-prove-test.render
  (:use :cl
        :cl-annot-prove
        :prove)
  (:import-from :cl-annot-prove.render
                :replace-call-next-method
                :render-method-chain
                :render-around
                :expected-formatter
                :extract-test-document
                :replace-test-form
                :render-symbol-tests)
  (:import-from :cl-annot-prove.struct
                :make-symbol-tests
                :symbol-tests-tests
                :make-test-document))
(in-package :cl-annot-prove-test.render)

(plan nil)

(subtest "replace-call-next-method"
  (is (replace-call-next-method '(let ((a 1)) (call-next-method)) '(= 1 a))
      '(let ((a 1)) (= 1 a))
      "can replace (call-next-method)."
      :test #'equal))

(subtest "render-method-chain"
  (let ((*default-test-function* #'equal))
    (is (render-method-chain '(= 1 1))
        '(= 1 1)
        "without any keywords.")

    (is (render-method-chain '(= 1 1)
                             :before '(print "before")
                             :after '(print "after"))
        `(progn (print "before")
                (= 1 1)
                (print "after"))
        "with :before, :after or both.")

    (is (render-method-chain '(= 1 a)
                             :around '(let ((a 1)) (call-next-method)))
        '(let ((a 1))
          (= 1 a))
        "with :around.")

    (is (render-method-chain '(= 1 a)
                             :before '(print "before")
                             :after '(print "after")
                             :around '(let ((a 1)) (call-next-method)))
        '(let ((a 1))
          (progn (print "before")
                 (= 1 a)
                 (print "after")))
        "with all keywords.")))


(subtest "render-around"
  (let ((symbol-tests (make-symbol-tests 'add '((= (add a b) c))
                                         :around '(let ((c 3)) (call-next-method))
                                         :around-each '(let ((a 1) (b 2)) (call-next-method)))))
    (is (render-around (car (symbol-tests-tests symbol-tests)) symbol-tests)
        '(let ((c 3))
          (let ((a 1)
                (b 2))
            (call-next-method)))
        "can render around."
        :test #'equal)))


(subtest "expected-formatter"
  (macrolet ((expected-formatter-test (op value expected)
               `(is (funcall (expected-formatter ',op) ,value)
                    ,expected
                    (format nil "~a." ',op))))
    (expected-formatter-test ok t "T")

    (expected-formatter-test is 1 "1")

    (expected-formatter-test isnt 1 "1")

    (expected-formatter-test is-values (list 1 2) "(VALUES 1 2)")

    (expected-formatter-test is-print "Hi" "PRINT Hi")

    (expected-formatter-test is-error 'simple-error "RAISE SIMPLE-ERROR")

    (expected-formatter-test is-type 'integer "TYPE: INTEGER")

    (expected-formatter-test like "[1-9]+" "LIKE: [1-9]+")

    (expected-formatter-test is-expand '(print "Sample") "EXPANDED TO: (PRINT \"Sample\")")))

(subtest "extract-test-document"
  (is (extract-test-document '(is 1 a) '(let ((a 1)) (call-next-method)))
      (make-test-document :got 1
                          :expected "1")
      "can extract test-document."
      :test #'equalp))

(subtest "replace-test-form"
  (is (princ-to-string (replace-test-form '(let ((a 1)) (is a b)) '(let ((b 1)) (call-next-method))))
      "(LET ((A 1))
  A
;; => 1)"
      "can extract test-document."
      :test #'equal))

(subtest "render-symbol-tests"
  (is (render-symbol-tests (make-symbol-tests 'add '((let ((a 1)) (is (add a b) c)))
                                              :before '(print "before")
                                              :after '(print "after")
                                              :around '(let ((c 3)) (call-next-method))
                                              :before-each '(print "before-each")
                                              :after-each '(print "after-each")
                                              :around-each '(let ((b 2)) (call-next-method))))
      (list "(LET ((C 3))
  (PROGN
   (PRINT \"before\")
   (LET ((B 2))
     (PROGN
      (PRINT \"before-each\")
      (LET ((A 1))
        (ADD A B)
;; => 3)
      (PRINT \"after-each\")))
   (PRINT \"after\")))")
      "can render symbol-tests."
      :test #'equal))

(finalize)
