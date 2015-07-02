(in-package :cl-user)
(defpackage sample
  (:use :cl
        :cl-annot-prove
        :prove))
(in-package :sample)

;; Have to load systems: cl-syntax and cl-syntax-annot.
(syntax:use-syntax :annot)

@tests
((is (add 1 2) 3))
(defun add (a b)
  (+ a b))

(render-symbol-tests (car (query-symbol-tests :symbol-name "ADD")))
#|
"(ADD 1 2)
;; => 3"
|#

@tests.before
(print "Before tests.")
@tests.after
(print "After tests.")
@tests.around
(let ((int1 1)) (call-next-method))
@tests.before.each
(print "Before each tests.")
@tests.after.each
(print "After each tests.")
@tests.around.each
(let ((int2 2)) (call-next-method))
@tests
((let ((int3 3))
   (is (add int1 int2) int3))
 (let ((int4 1))
   (is (add int1 int4) int2)))
(defun add (a b)
  (+ a b))

(render-symbol-tests (car (query-symbol-tests :symbol-name "ADD")))
#|
"(LET ((INT1 1))
  (PROGN
   (PRINT \"Before tests.\")
   (LET ((INT2 2))
     (PROGN
      (PROGN
       (PRINT \"Before each tests.\")
       (LET ((INT3 3))
         (ADD INT1 INT2)
;; => 3)
       (PRINT \"After each tests.\"))
      (PROGN
       (PRINT \"Before each tests.\")
       (LET ((INT4 1))
         (ADD INT1 INT4)
;; => 2)
       (PRINT \"After each tests.\"))))
   (PRINT \"After tests.\")))"
|#

(run-symbol-tests (car (query-symbol-tests :symbol-name "ADD")))
#|
"Before tests." 
"Before each tests." 
  ✓ 3 is expected to be 3 

"After each tests." 
"Before each tests." 
  ✓ 2 is expected to be 2 

"After each tests." 
"After tests." 
"After tests."
|#

(run-package-tests *package*)
#|
 PACKAGE: #<PACKAGE "SAMPLE">
1..1

 SYMBOL: ADD

"Before tests." 
    ✓ 3 is expected to be 3 

"After tests." 
"Before tests." 
    ✓ 2 is expected to be 2 

"After tests." 
✓ 1 test completed (0ms)
T
|#
