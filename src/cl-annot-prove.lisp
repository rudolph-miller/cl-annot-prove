(in-package :cl-user)
(defpackage cl-annot-prove
  (:import-from :cl-annot-prove.struct
                :*symbol-tests-list*
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
                :test-document
                :test-document-got
                :test-documest-expected)
  (:import-from :cl-annot-prove.render
                :render-symbol-tests)
  (:import-from :cl-annot-prove.helper
                :query-symbol-tests
                :run-symbol-tests
                :run-package-tests)
  (:import-from :cl-annot-prove.annotation
                :tests)
  (:export ;; struct
           :*symbol-tests-list*
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
           :test-document
           :test-document-got
           :test-document-expected

           ;; render
           :render-symbol-tests

           ;; helper
           :query-symbol-tests
           :run-symbol-tests
           :run-package-tests

           ;; annotation
           :tests))

