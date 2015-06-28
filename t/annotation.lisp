(in-package :cl-user)
(defpackage cl-annot-prove-test.annotation
  (:use :cl
        :cl-annot-prove
        :prove))
(in-package :cl-annot-prove-test.annotation)

(plan nil)

(subtest "tests"
  (skip 1 "Not written yet."))

(finalize)
