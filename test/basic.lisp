(in-package :forth-cl/tests)

(defmacro with-empty-stack (&rest args)
  `(progn
     (setf forth-cl::*stack* nil)
     ,@args))

(define-test can-put-number-on-stack
  (with-empty-stack
      (forth "1")
    (is equal '(1) forth-cl::*stack*)))

(define-test can-swap-numbers-on-stack
  (with-empty-stack
      (forth "1 2 SWAP")
    (is equal '(1 2) forth-cl::*stack*)))

(define-test can-drop-number-from-stack
  (with-empty-stack
      (forth "1 2 DROP")
    (is equal '(1) forth-cl::*stack*)))

(define-test can-duplicate-number-on-stack
  (with-empty-stack
      (forth "42 DUP")
    (is equal '(42 42) forth-cl::*stack*)))

(define-test can-add-numbers-on-stack
  (with-empty-stack
      (forth "4 5 +")
    (is equal '(9) forth-cl::*stack*)))

(define-test can-subtract-numbers-on-stack
  (with-empty-stack
      (forth "5 1 -")
    (is equal '(4) forth-cl::*stack*)))

(define-test can-multiply-numbers-on-stack
  (with-empty-stack
      (forth "5 2 *")
    (is equal '(10) forth-cl::*stack*)))

(define-test can-divide-numbers-on-stack
  (with-empty-stack
      (forth "10 2 /")
    (is equal '(5) forth-cl::*stack*)))

(define-test can-read-word-from-stdin
  (with-empty-stack
      (forth "WORD hello")
    (is equal '("hello") forth-cl::*stack*)))

(define-test can-read-number-from-stdin
  (with-empty-stack
      (forth "NUMBER 2")
    (is equal '(2) forth-cl::*stack*)))

(define-test fails-to-read-non-number-from-stdin
  (handler-case (forth "NUMBER ABC")
    (not-a-number (c)
      (is equal "ABC" (not-a-number-word c)))))

(define-test can-read-char-from-stdin
  (with-empty-stack
      (forth "KEY a")
    (is equal '(#\A) forth-cl::*stack*)))

