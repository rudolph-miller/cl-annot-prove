#|
  This file is a part of cl-annot-prove project.
  Copyright (c) 2015 Rudolph Miller (chopsticks.tk.ppfm@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-annot-prove-test-asd
  (:use :cl :asdf))
(in-package :cl-annot-prove-test-asd)

(defsystem cl-annot-prove-test
  :author "Rudolph Miller"
  :license "MIT"
  :homepage "https://github.com/Rudolph-Miller/cl-annot-prove"
  :depends-on (:cl-annot-prove
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-annot-prove"))))
  :description "Test system for cl-annot-prove."

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
