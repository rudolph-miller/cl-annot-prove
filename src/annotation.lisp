(in-package :cl-user)
(defpackage cl-annot-prove.annotation
  (:use :cl
        :annot.util
        :annot.helper
        :cl-annot-prove.struct
        :cl-annot-prove.helper)
  (:export :tests
           :tests.before
           :tests.after
           :tests.around
           :tests.before.each
           :tests.after.each
           :tests.around.each))
(in-package :cl-annot-prove.annotation)

(defmacro deftests-annotation (name accessor)
  (let ((definition-form (gensym "definition-form"))
        (form (gensym "form"))
        (symbol (gensym "symbol"))
        (symbol-tests (gensym "symbol-tests")))
    `(defannotation ,name (,form ,definition-form)
         (:arity 2)
       (let* ((,symbol (definition-form-symbol ,definition-form))
              (,symbol-tests (or (car (query-symbol-tests :symbol ,symbol))
                                (make-symbol-tests ,symbol))))
         (setf (,accessor ,symbol-tests) ,form)
         (add-symbol-tests ,symbol-tests)
         ,definition-form))))

(deftests-annotation tests symbol-tests-tests)
(deftests-annotation tests.before symbol-tests-before)
(deftests-annotation tests.after symbol-tests-after)
(deftests-annotation tests.around symbol-tests-around)
(deftests-annotation tests.before.each symbol-tests-before-each)
(deftests-annotation tests.after.each symbol-tests-after-each)
(deftests-annotation tests.around.each symbol-tests-around-each)
