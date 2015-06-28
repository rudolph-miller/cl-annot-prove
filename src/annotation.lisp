(in-package :cl-user)
(defpackage cl-annot-prove.annotation
  (:use :cl
        :annot.util
        :annot.helper
        :cl-annot-prove.struct)
  (:export :tests))
(in-package :cl-annot-prove.annotation)

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
