(in-package :cl-user)
(defpackage cl-annot-prove.helper
  (:use :cl
        :annot.doc
        :cl-annot-prove.struct)
  (:export :query-symbol-tests))
(in-package :cl-annot-prove.helper)

(syntax:use-syntax :annot)

@doc
"Return list of #S(SYMBOL-TESTS ...)s which match the conditions."
(defun query-symbol-tests (&key symbol symbol-name symbol-package (symbol-tests-list *symbol-tests-list*))
  (labels ((equal-symbol-name (symbol symbol-name)
             (equal (symbol-name symbol) symbol-name))
           (equal-symbol-package-name (symbol package-name)
             (let ((symbol-package (symbol-package symbol)))
               (or (equal (package-name symbol-package) package-name)
                   (member package-name (package-nicknames symbol-package) :test #'equal)))))
    (loop with symbol-package-name = (when symbol-package
                                       (package-name (if (typep symbol-package 'package)
                                                         symbol-package
                                                         (find-package symbol-package))))
          for tests in symbol-tests-list
          for tests-symbol = (symbol-tests-symbol tests)
          when (cond
                 (symbol
                  (eql tests-symbol symbol))
                 ((and symbol-name symbol-package)
                  (and (equal-symbol-name tests-symbol symbol-name)
                       (equal-symbol-package-name tests-symbol symbol-package-name)))
                  (symbol-name
                   (equal-symbol-name tests-symbol symbol-name))
                  (symbol-package
                   (equal-symbol-package-name tests-symbol symbol-package-name))
                  (t nil))
                 collecting tests)))
