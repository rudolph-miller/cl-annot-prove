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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; render

(defun extract-test-document (form)
  (when (listp form)
    (let ((expected (extract-test-expected (car form) form)))
      (when expected
    (make-test-document :got (cadr form)
                        :expected expected)))))

(defgeneric extract-test-expected (operator form)
  (:method (operator form)
    (declare (ignore operator form))
    nil))

(defmacro def-extract-test-expected ((symbol form) &body body)
  (let ((operator (gensym "operator")))
    `(defmethod extract-test-expected ((,operator (eql ',symbol)) ,form)
       (declare (ignore ,operator))
       ,@body)))

(def-extract-test-expected (prove:ok form)
  "T")

(def-extract-test-expected (prove:is form)
  (format nil "~s" (caddr form)))

(def-extract-test-expected (prove:isnt form)
  (format nil "~s" (caddr form)))

(def-extract-test-expected (prove:is-values form)
  (format nil "(VALUES ~{~s~^ ~})" (caddr form)))

(def-extract-test-expected (prove:is-print form)
  (format nil "PRINT ~a" (caddr form)))

(def-extract-test-expected (prove:is-error form)
  (format nil "RAISE ~a" (caddr form)))

(def-extract-test-expected (prove:is-type form)
  (format nil "TYPE: ~a" (caddr form)))

(def-extract-test-expected (prove:like form)
  (format nil "LIKE: ~s" (caddr form)))

(def-extract-test-expected (prove:is-expand form)
  (format nil "EXPANDED TO: ~s" (caddr form)))

(defun replace-test-form (test-form)
  (or (extract-test-document test-form)
      (if (listp test-form)
          (mapcar #'(lambda (item)
                      (let ((result (replace-test-form item)))
                        (if (typep result 'test-document)
                            result
                            item)))
                  test-form)
          test-form)))

(defun replace-call-next-method (form new)
  (if (listp form)
      (if (equal form '(cl:call-next-method))
          new
          (mapcar #'(lambda (item)
                      (replace-call-next-method item new))
                  form))
      form))
  
(defun render-method-chain (main &key before after around)
  (let ((inner (if (or before after)
                   `(progn ,@(when before (list before))
                           ,main
                           ,@(when after (list after)))
                   main)))
    (if around
        (replace-call-next-method around inner)
        inner)))

(defun render-symbol-tests (symbol-tests)
  (labels ((render-around (test)
           (render-method-chain (or (test-around test) '(cl:call-next-method))
                                :before (symbol-tests-before symbol-tests)
                                :after (symbol-tests-after symbol-tests) :around (symbol-tests-around symbol-tests)))
           (render-test (test)
             (render-method-chain (replace-test-form (test-form test))
                                :around (render-around test))))
    (mapcar #'(lambda (test)
                (princ-to-string (render-test test)))
            (symbol-tests-tests symbol-tests))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; annotation

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
