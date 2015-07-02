(in-package :cl-user)
(defpackage cl-annot-prove-test.helper
  (:use :cl
        :cl-annot-prove
        :prove))
(in-package :cl-annot-prove-test.helper)

(plan nil)


(tests ((is (add 1 2) 3)
        (is (add 2 3) 5))
       (defun add (a b)
         (+ a b)))

(subtest "query-symbol-tests"
  (macrolet ((found (args comment)
               `(ok (>= (length (query-symbol-tests ,@args)) 1)
                    ,comment))

             (not-found (args comment)
               `(is (query-symbol-tests ,@args)
                    nil
                    ,comment)))
    (subtest "found"
      (found (:symbol 'add)
             "with :symbol.")

      (found (:symbol-name "ADD")
             "with :symbol-name.")

      (found (:symbol-package "CL-ANNOT-PROVE-TEST.HELPER")
             "with :symbol-package string.")

      (found (:symbol-package *package*)
             "with :symbol-package package."))

    (subtest "not found"
      (not-found (:symbol 'not-found)
                 "with :symbol.")

      (not-found (:symbol-name "NOT-FOUND")
                 "with :symbol-name."))

      (not-found (:symbol-package "CL-ANNOT-PROVE")
             "with :symbol-package string.")

      (not-found (:symbol-package (find-package :cl-annot-prove))
             "with :symbol-package package.")))

(subtest "run-symbol-tests"
  (ok (with-output-to-string (*standard-output*)
        (run-symbol-tests (car (query-symbol-tests :symbol 'add))))
      "can run symbol-tests-tests."))

(subtest "run-package-tests"
  (ok (with-output-to-string (*standard-output*)
        (run-package-tests *package*))
      "can run package tests."))

(subtest "run-system-tests"
  (ok (with-output-to-string (*standard-output*)
        (run-system-tests :cl-annot-prove))
      "can run system tests."))

(finalize)
