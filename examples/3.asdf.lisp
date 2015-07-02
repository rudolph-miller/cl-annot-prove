(in-package :cl-user)
(defpackage sample-asd
  (:use :cl :asdf))
(in-package :sample-asd)

(defsystem sample
  :version "0.1"
  :author "Rudolph Miller"
  :license "MIT"
  :depends-on (:cl-syntax
               :cl-syntax-annot
               :cl-annot-prove)
  :components ((:module "src"
                :serial t
                :components
                ((:file "sample"))))
  :description "Sample asd."
  :perform (test-op (op c)
                    (uiop:symbol-call :cl-annot-prove :run-system-tests c)))
;; Add two lines from the bottom.

#|
(asdf:test-system :sample)

...

✓ 1 test completed (0ms)
T
|#
