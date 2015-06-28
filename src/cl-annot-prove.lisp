(in-package :cl-user)
(defpackage cl-annot-prove
  (:use :cl
        :annot.doc
        :annot.util
        :annot.helper
        :prove)
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
                :make-symbol-tests
                :test-document
                :test-document-got
                :test-documest-expected
                :make-test-document)
  (:import-from :cl-annot-prove.helper
                :query-symbol-tests)
  (:import-from :cl-annot-prove.render
                :render-symbol-tests)
  (:export :*symbol-tests-list*
           :query-symbol-tests
           :test-form
           :test-before
           :test-after
           :test-around
           :symbol-tests-symbol
           :symbol-tests-tests
           :symbol-tests-before
           :symbol-tests-after
           :symbol-tests-around
           :test-document
           :test-document-got
           :test-document-expected
           :render-symbol-tests
           :tests))
(in-package :cl-annot-prove)

(syntax:use-syntax :annot)


(defannotation tests (test-forms definition-form)
    (:arity 2)
  (progn-form-replace-last
   (lambda (definition-form)
     (case (definition-form-type definition-form)
       ((defun defmacro)
        (let ((symbol (cadr definition-form)))
          (make-symbol-tests symbol test-forms))
        definition-form)
       (otherwise (error "Test not supported: ~a" definition-form))))
   definition-form))

#|
Tasks:
- tests.before
- tests.after
- tests.around
- tests.before-each
- tests.after-each
- tests.around-each
|#
