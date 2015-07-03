(in-package :cl-user)
(defpackage cl-annot-prove-test.render
  (:use :cl
        :cl-annot-prove
        :prove)
  (:import-from :cl-annot-prove.render
                :stub-progn
                :replace-call-tests
                :render-method-chain
                :render-around
                :expected-formatter
                :replace-test-with-setq-form
                :replace-test-form
                :replace-stub-progn
                :render-symbol-tests)
  (:import-from :cl-annot-prove.struct
                :make-symbol-tests))
(in-package :cl-annot-prove-test.render)

(plan nil)

(subtest "replace-call-tests"
  (is (replace-call-tests '(let ((a 1)) (call-tests)) '(= 1 a))
      '(let ((a 1)) (= 1 a))
      "can replace (call-tests)."
      :test #'equal))

(subtest "render-method-chain"
  (let ((*default-test-function* #'equal))
    (is (render-method-chain '(= 1 1))
        '(= 1 1)
        "without any keywords.")

    (is (render-method-chain '(= 1 1)
                             :before '(print "before")
                             :after '(print "after"))
        `(stub-progn (print "before")
           (= 1 1)
           (print "after"))
        "with :before, :after or both.")

    (is (render-method-chain '(= 1 a)
                             :around '(let ((a 1)) (call-tests)))
        '(let ((a 1))
          (= 1 a))
        "with :around.")

    (is (render-method-chain '(= 1 a)
                             :before '(print "before")
                             :after '(print "after")
                             :around '(let ((a 1)) (call-tests)))
        '(let ((a 1))
          (stub-progn (print "before")
            (= 1 a)
            (print "after")))
        "with all keywords.")))


(subtest "render-around"
  (let ((symbol-tests (make-symbol-tests 'add
                                         :tests '((= (add a b) c))
                                         :around '(let ((c 3)) (call-tests))
                                         :around-each '(let ((a 1) (b 2)) (call-tests)))))
    (is (render-around symbol-tests)
        '(let ((c 3))
          (let ((a 1)
                (b 2))
            (call-tests)))
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

(subtest "replace-test-with-setq-form"
  (multiple-value-bind (replaced-form results) (replace-test-with-setq-form '(let ((a 1)) (is a b)))
    (let ((result-symbol (caar results)))
      (is replaced-form
          `(let ((a 1)) (setq ,result-symbol b))
          "can return replaced form and can store symbols in results.")

      (let ((result (cdr (car results))))
        (is (getf result :got)
            'a
            "can store :got.")

        (is (getf result :setq)
            `(setq ,result-symbol b)
            "can store :setq.")

        (is-type (getf result :formatter)
                 'function
                 "can store :formatter.")))))

(subtest "replace-test-form"
  (is (princ-to-string (replace-test-form '(let ((a 1)) (is a b)) '(let ((b 1)) (call-tests))))
      "(LET ((A 1))
  A
;; => 1)"
      "can replace."
      :test #'equal))

(subtest "replace-stub-progn"
  (is (replace-stub-progn '(stub-progn (print 1) (print 2)))
      '(progn (print 1) (print 2))
      "in the toplevel.")

  (is (replace-stub-progn `(stub-progn (let ((a 1)) (stub-progn (print a) (print 1)))))
      '(progn (let ((a 1)) (print a) (print 1)))
      "not in the toplevel."))

(subtest "render-symbol-tests"
  (is (render-symbol-tests (make-symbol-tests 'add
                                              :tests '((let ((a 1)) (is (add a b) c)))
                                              :before '(print "before")
                                              :after '(print "after")
                                              :around '(let ((c 3)) (call-tests))
                                              :before-each '(print "before-each")
                                              :after-each '(print "after-each")
                                              :around-each '(let ((b 2)) (call-tests))))
      "(LET ((C 3))
  (PRINT \"before\")
  (LET ((B 2))
    (PRINT \"before-each\")
    (LET ((A 1))
      (ADD A B)
;; => 3)
    (PRINT \"after-each\"))
  (PRINT \"after\"))"
      "can render symbol-tests."
      :test #'equal))

(finalize)
