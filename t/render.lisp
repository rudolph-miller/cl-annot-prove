(in-package :cl-user)
(defpackage cl-annot-prove-test.render
  (:use :cl
        :cl-annot-prove
        :prove))
(in-package :cl-annot-prove-test.render)

(plan nil)

(subtest "render-symbol-tests"
  (skip 1 "Not written yet."))

(finalize)