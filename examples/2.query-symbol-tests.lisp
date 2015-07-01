(in-package :cl-user)
(defpackage sample
  (:use :cl
        :cl-annot-prove
        :prove))
(in-package :sample)

(syntax:use-syntax :annot)

@tests
((is (add 1 2) 3))
(defun add (a b)
  (+ a b))

(query-symbol-tests :symbol 'add)
;; => (list #S(SYMBOL-TESTS ...) ...)

(query-symbol-tests :symbol-name "ADD")
;; => (list #S(SYMBOL-TESTS ...) ...)

(query-symbol-tests :symbol-package (symbol-package 'add))
;; => (list #S(SYMBOL-TESTS ...) ...)

(query-symbol-tests :symbol-package :sample)
;; => (list #S(SYMBOL-TESTS ...) ...)

(query-symbol-tests :symbol-package "SAMPLE")
;; => (list #S(SYMBOL-TESTS ...) ...)
