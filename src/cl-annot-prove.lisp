(in-package :cl-user)
(uiop:define-package cl-annot-prove
  (:nicknames :annot.prove)
  (:use-reexport :prove)
  (:import-from :cl-annot-prove.struct
                :*symbol-tests-list*
                :symbol-tests
                :symbol-tests-symbol
                :symbol-tests-tests
                :symbol-tests-before
                :symbol-tests-after
                :symbol-tests-around
                :symbol-tests-before-each
                :symbol-tests-after-each
                :symbol-tests-around-each
                :symbol-tests-load-pathname
                :test-document
                :test-document-got
                :test-document-expected)
  (:import-from :cl-annot-prove.render
                :render-symbol-tests)
  (:import-from :cl-annot-prove.helper
                :query-symbol-tests
                :run-symbol-tests
                :run-package-tests
                :system-symbol-tests-list
                :run-system-tests)
  (:import-from :cl-annot-prove.annotation
                :tests
                :tests.before
                :tests.after
                :tests.around
                :tests.before.each
                :tests.after.each
                :tests.around.each)
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
           :symbol-tests-before-each
           :symbol-tests-after-each
           :symbol-tests-around-each
           :symbol-tests-load-pathname
           :test-document
           :test-document-got
           :test-document-expected

           ;; render
           :render-symbol-tests

           ;; helper
           :query-symbol-tests
           :run-symbol-tests
           :run-package-tests
           :system-symbol-tests-list
           :run-system-tests

           ;; annotation
           :tests
           :tests.before
           :tests.after
           :tests.around
           :tests.before.each
           :tests.after.each
           :tests.around.each))
