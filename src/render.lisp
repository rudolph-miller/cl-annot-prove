(in-package :cl-user)
(defpackage cl-annot-prove.render
  (:use :cl
        :annot.doc
        :cl-annot-prove.struct)
  (:export :replace-call-next-method
           :render-method-chain
           :render-around
           :expected-formatter
           :extract-test-document
           :replace-test-form
           :render-symbol-tests))
(in-package :cl-annot-prove.render)

(syntax:use-syntax :annot)

(defun replace-call-next-method (form new)
  (if (listp form)
      (if (equal form '(cl:call-next-method))
          new
          (mapcar #'(lambda (item)
                      (replace-call-next-method item new))
                  form))
      form))

(defun render-method-chain (main &key before after around)
  (let ((inner (if (or before after)
                   `(progn ,@(when before (list before))
                           ,main
                           ,@(when after (list after)))
                   main)))
    (if around
        (replace-call-next-method around inner)
        inner)))

(defun render-around (test symbol-tests)
  (render-method-chain (or (test-around test) '(cl:call-next-method))
                       :before (symbol-tests-before symbol-tests)
                       :after (symbol-tests-after symbol-tests)
                       :around (symbol-tests-around symbol-tests)))

(defgeneric expected-formatter (operator)
  (:method (operator)
    (declare (ignore operator))
    nil))

(defmacro def-expected-formatter ((symbol expected) &body body)
  (let ((operator (gensym "operator")))
    `(defmethod expected-formatter ((,operator (eql ',symbol)))
       (declare (ignore ,operator))
       (lambda (,expected)
         ,@body))))

(def-expected-formatter (prove:ok expected)
  (declare (ignore expected))
  "T")

(def-expected-formatter (prove:is expected)
  (format nil "~s" expected))

(def-expected-formatter (prove:isnt expected)
  (format nil "~s" expected))

(def-expected-formatter (prove:is-values expected)
  (format nil "(VALUES ~{~s~^ ~})" expected))

(def-expected-formatter (prove:is-print expected)
  (format nil "PRINT ~a" expected))

(def-expected-formatter (prove:is-error expected)
  (format nil "RAISE ~a" expected))

(def-expected-formatter (prove:is-type expected)
  (format nil "TYPE: ~a" expected))

(def-expected-formatter (prove:like expected)
  (format nil "LIKE: ~a" expected))

(def-expected-formatter (prove:is-expand expected)
  (format nil "EXPANDED TO: ~s" expected))

(defun get-inner-value (inner around)
  (let* ((*standard-output* (make-broadcast-stream))
         (result (gensym "result"))
         (result-inner `(setq ,result ,inner))
         (result-around `(let (,result) (call-next-method) ,result)))
    (handler-bind ((warning (lambda (c)
                                    (declare (ignore c))
                                    (muffle-warning))))
      (eval
       (render-method-chain (render-method-chain result-inner :around around)
                            :around result-around)))))

(defun extract-test-document (form around)
  (when (listp form)
    (let* ((formatter (expected-formatter (car form))))
      (when formatter
        (let ((expected (funcall formatter (get-inner-value (caddr form) around))))
          (make-test-document :got (cadr form)
                              :expected expected))))))

(defun replace-test-form (test-form around)
  (or (extract-test-document test-form around)
      (if (listp test-form)
          (mapcar #'(lambda (item)
                      (let ((result (replace-test-form item around)))
                        (if (typep result 'test-document)
                            result
                            item)))
                  test-form)
          test-form)))

@doc
"Render #S(SYMBOL-TESTS ...) for documents."
(defun render-symbol-tests (symbol-tests)
  (mapcar #'(lambda (test)
              (let ((around (render-around test symbol-tests)))
                (format nil "~s"
                        (render-method-chain (replace-test-form (test-form test) around)
                                             :before (test-before test)
                                             :after (test-after test)
                                             :around around))))
          (symbol-tests-tests symbol-tests)))
