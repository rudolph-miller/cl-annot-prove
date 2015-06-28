(in-package :cl-user)
(defpackage cl-annot-prove-test.helper
  (:use :cl
        :cl-annot-prove
        :prove))
(in-package :cl-annot-prove-test.helper)

(plan nil)

(subtest "query-symbol-tests"
  (skip 1 "Not written yet."))

(finalize)
