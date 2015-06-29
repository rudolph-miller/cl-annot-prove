(in-package :cl-user)
(defpackage cl-annot-prove-test.render
  (:use :cl
        :cl-annot-prove
        :prove)
  (:import-from :cl-annot-prove.render
                :render-around))
(in-package :cl-annot-prove-test.render)

(plan nil)

(subtest "extract-test-expected"
  (skip 1 "Not written yet."))

(subtest "extract-test-document"
  (skip 1 "Not written yet."))

(subtest "replace-test-form"
  (skip 1 "Not written yet."))

(subtest "replace-call-next-method"
  (skip 1 "Not written yet."))

(subtest "render-around"
  (skip 1 "Not written yet."))

(subtest "render-around"
  (skip 1 "Not written yet."))

(subtest "render-symbol-tests"
  (skip 1 "Not written yet."))

(finalize)
