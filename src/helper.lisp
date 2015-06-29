(in-package :cl-user)
(defpackage cl-annot-prove.helper
  (:use :cl
        :annot.doc
        :prove
        :cl-annot-prove.struct
        :cl-annot-prove.render)
  (:export :query-symbol-tests
           :run-symbol-tests
           :run-package-tests))
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

@doc
"Run symbol-tests."
(defun run-symbol-tests (symbol-tests)
  (let ((tests (symbol-tests-tests symbol-tests)))
    (dolist (test tests)
      (eval
       (render-method-chain (test-form test)
                            :around (render-around test symbol-tests))))))

@doc
"Run symbol-tests in the package."
(defun run-package-tests (package)
  (let ((symbol-tests-list (query-symbol-tests :symbol-package package)))
    (diag (format nil "PACKAGE: ~a" package))
    (plan (length symbol-tests-list))
    (dolist (symbol-tests symbol-tests-list)
      (subtest (format nil "SYMBOL: ~a" (symbol-tests-symbol symbol-tests))
        (run-symbol-tests symbol-tests)))
    (finalize)))

