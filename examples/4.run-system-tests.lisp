(asdf:load-system :sample)

(annot.prove:run-system-tests :sample)
#|
...

✓ 1 test completed (0ms)
T
|#

;; or you can use roswell script
;; $ ros install cl-annot-prove
;; $ run-annot-prove *.asd
