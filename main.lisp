(defpackage :forth-cl
  (:use :cl))

(in-package :forth-cl)

(defmacro forth (&rest input)
  `(process-from-lisp '(,@input)))

(defparameter *stack* nil)

(defmacro put-word (word fun ht)
  `(setf (gethash ,word ,ht) (lambda () ,fun)))

(defmacro put-numop2 (sym fun ht)
  `(put-word ,sym (num-op-2 ,fun) ,ht))

(defun peek-stack ()
  (when *stack* (car *stack*)))

(defun pop-stack ()
  (let ((result (peek-stack)))
    (when result (setf *stack* (cdr *stack*)))
    result))

(defun push-stack (item)
  (when item (push item *stack*)))

(defun num-op-2 (op)
  (let ((a (pop-stack))
        (b (pop-stack)))
    (if (and a b)
        (push-stack (apply op (list a b)))
        (if b
            (error 'missing-stack-value)
            (error 'empty-stack)))))

(defun dup ()
  (let ((a (peek-stack)))
    (when a (push-stack a))))

(defun swap ()
  (let ((a (pop-stack))
        (b (pop-stack)))
    (when (and a b)
        (push-stack a)
        (push-stack b))))

(defstruct forth-variable value)

(defparameter *variables* (make-hash-table :test 'equal))

(defun var-store ()
  (let ((var (pop-stack))
        (value (pop-stack)))
    (if (and var value)
        (setf (forth-variable-value var) value)
        (error 'missing-stack-value))))

(defun var-fetch ()
  (push-stack (forth-variable-value (pop-stack))))

(defparameter *words*
  (let ((ht (make-hash-table :test 'equal)))
    (put-numop2 "+" #'+ ht)
    (put-numop2 "*" #'* ht)
    (put-numop2 "-" #'- ht)
    (put-numop2 "/" #'/ ht)
    (put-word "." (format t "~A~%" *stack*) ht)
    (put-word "DROP" (pop-stack) ht)
    (put-word "DUP" (dup) ht)
    (put-word "SWAP" (swap) ht)
    (put-word "!" (var-store) ht)
    (put-word "@" (var-fetch) ht)
    ht))

(defun var-define (name)
  (let ((var (make-forth-variable)))
    (setf (gethash name *variables*) var)
    (put-word name (push-stack var) *words*)))

(defun tick (name)
  (let ((fun (gethash name *words*)))
    (if fun
        (push-stack fun)
        (error 'name-not-defined (format nil "undefined name: ~A~%" name)))))

(defun exec-fun ()
  (apply (pop-stack) nil))

;; execute modes are:
;;    - exec: execute word
;;    - var: define a variable
;;    - tick: get word's function
(defparameter *execute-mode* :exec)

(defmacro restoring-exec (&rest forms)
  `(progn ,@forms (setf *execute-mode* :exec)))

(defun process-number (word)
  (if (numberp word)
      (push-stack word)
      (let ((n (ignore-errors (parse-integer word))))
        (if n
            (push-stack n)
            (error :not-forth-type (format nil "not a Forth value: ~A~%" word))))))

(defun process(words)
  (dolist (word words)
    (cond
      ((eq *execute-mode* :var)
       (restoring-exec (var-define word)))
      ((eq *execute-mode* :tick)
       (restoring-exec (tick word)))
      ;; :exec mode
      ((equal word "VARIABLE")
       (setf *execute-mode* :var))
      ((equal word "EXECUTE")
       (exec-fun))
      ((equal word "'")
       (setf *execute-mode* :tick))
      (t (let ((op (gethash word *words*)))
              (if op
                  (apply op nil)
                  (process-number word)))))))

(defun process-from-lisp (&rest items)
  (dolist (item items)
    (cond ((listp item)
           (apply #'process-from-lisp item))
          ((symbolp item) (process (list (symbol-name item))))
          ((numberp item) (process-number item))
          ((stringp item) (process (list item)))
          (t (error :invalid-value (format nil "forth cannot handle value: ~A" item))))))
