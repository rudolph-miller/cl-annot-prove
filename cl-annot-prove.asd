#|
  This file is a part of cl-annot-prove project.
  Copyright (c) 2015 Rudolph Miller (chopsticks.tk.ppfm@gmail.com)
|#

#|
  Annotation Syntax Test Library.
  Author: Rudolph Miller (chopsticks.tk.ppfm@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-annot-prove-asd
  (:use :cl :asdf))
(in-package :cl-annot-prove-asd)

(defsystem cl-annot-prove
  :version "0.1"
  :author "Rudolph Miller"
  :license "MIT"
  :homepage "https://github.com/Rudolph-Miller/cl-annot-prove"
  :depends-on (:cl-syntax
               :cl-syntax-annot
               :prove)
  :components ((:module "src"
                :serial t
                :components
                ((:file "struct")
                 (:file "helper")
                 (:file "render")
                 (:file "cl-annot-prove"))))
  :description "Annotation Syntax Test Library."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-annot-prove-test))))
