(in-package :cl-user)
(defpackage sample
  (:use :cl
        :cl-annot-prove
        :prove))
(in-package :sample)

;; Have to load systems: cl-syntax and cl-syntax-annot.
(syntax:use-syntax :annot)

@tests
((is (add 1 2) 3)
 (is (add 2 3) 5))
(defun add (a b)
  (+ a b))

(render-symbol-tests (car (query-symbol-tests :symbol-name "ADD")))
#|
("(ADD 1 2)
;; => 3"
 "(ADD 2 3)
;; => 5")
|#
