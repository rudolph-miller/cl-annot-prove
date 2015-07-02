(in-package :cl-user)
(defpackage cl-annot-prove.helper
  (:use :cl
        :annot.doc
        :prove
        :cl-annot-prove.struct
        :cl-annot-prove.render)
  (:import-from :cl-fad
                :walk-directory)
  (:import-from :prove.asdf
                :*last-suite-report*)
  (:export :query-symbol-tests
           :run-symbol-tests
           :run-package-tests
           :system-symbol-tests-list
           :run-system-tests))
(in-package :cl-annot-prove.helper)

(syntax:use-syntax :annot)

@doc
"Return list of #S(SYMBOL-TESTS ...)s which match the conditions."
(defun query-symbol-tests (&key symbol symbol-name symbol-package (symbol-tests-list *symbol-tests-list*))
  (labels ((equal-symbol-name (symbol symbol-name)
             (equal (symbol-name symbol) symbol-name))
           (equal-symbol-package-name (symbol package-name)
             (let ((symbol-package (symbol-package symbol)))
               (when symbol-package
                 (or (equal (package-name symbol-package) package-name)
                     (member package-name (package-nicknames symbol-package) :test #'equal))))))
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
       (render-method-chain test
                            :around (render-around symbol-tests))))))

(defun run-symbol-tests-list (symbol-tests-list)
  (plan (length symbol-tests-list))
  (dolist (symbol-tests symbol-tests-list)
    (subtest (symbol-name (symbol-tests-symbol symbol-tests))
      (run-symbol-tests symbol-tests)))
  (finalize))

@doc
"Run symbol-tests in the package."
(defun run-package-tests (package)
  (let ((symbol-tests-list (query-symbol-tests :symbol-package package))
        (*last-suite-report* nil))
    (diag (format nil "PACKAGE: ~a" (if (typep package 'package)
                                        (package-name package)
                                        package)))
    (run-symbol-tests-list symbol-tests-list)
    (unless *last-suite-report*
      (warn "Test completed without 'finalize'd."))
    (= (getf *last-suite-report* :failed) 0)))

(defun load-system-silestly (system-designator)
  (let ((system (if (typep system-designator 'asdf:system)
                    (asdf:component-name system-designator)
                    system-designator)))
    #+quicklisp (ql:quickload system :silent t)
    (handler-bind ((warning (lambda (c)
                              (declare (ignore c))
                              (muffle-warning))))
      (asdf:load-system system :force t))))

@doc
"Return list of #S(SYMBOL-TESTS ...)s in the system."
(defun system-symbol-tests-list (system-designator &key (reload-system t))
  (when reload-system
    (load-system-silestly system-designator))
  (let* ((source-directory (asdf:system-source-directory system-designator))
         (source-files))
    (walk-directory source-directory #'(lambda (pathname) (push pathname source-files)))
    (loop for symbol-tests in *symbol-tests-list*
          when (member (symbol-tests-load-pathname symbol-tests) source-files :test #'equal)
            collecting symbol-tests)))

@doc
"Run symbol-tests in the system."
(defun run-system-tests (system-designator &key (reload-system t))
  (let* ((should-test-packages))
    (flet ((add-should-test-package (package)
             (unless (member package should-test-packages :test #'equal)
               (push package should-test-packages))))
      (dolist (symbol-tests (system-symbol-tests-list system-designator :reload-system reload-system))
        (add-should-test-package (symbol-package (symbol-tests-symbol symbol-tests))))
      (loop for package in should-test-packages
            always (run-package-tests package)))))

