(defpackage :forth-cl
  (:use :cl))

(in-package :forth-cl)

(defmacro forth (&rest input)
  "The main Forth macro for writing Forth in Lisp"
  `(process-from-lisp '(,@input)))

(defparameter *stack* nil
  "The Forth data stack")

;; A few Forth interpreter data structures

(defstruct forth-variable value)

(defstruct forth-word
  (function (lambda ()) :type function)
  (immediate nil    :type boolean)
  (description ""   :type string))

(defparameter *forth-memory*
  (make-array (* 10 1024) :fill-pointer 0 :adjustable t))

;; execute modes are:
;;    - immediate: fetch and immediately execute words.
;;    - compile: fetch word, leave it in memory.
(deftype forth-mode-t () '(member :immediate :compile))
(declaim (type (forth-mode-t) *forth-mode*))
(defparameter *forth-mode* :immediate)

;;; Macros to help define the "code words" written in Lisp

(defmacro put-word (word fun ht)
  `(setf (gethash ,word ,ht)
         (make-forth-word :function (lambda () ,fun))))

(defmacro put-word! (word fun desc immediate ht)
  `(setf (gethash ,word ,ht)
         (make-forth-word :function (lambda () ,fun)
                          :description ,desc
                          :immediate ,immediate)))

(defmacro put-numop2 (sym fun ht)
  `(put-word ,sym (num-op-2 ,fun) ,ht))

;;; Stack fundamental operations.

(defun peek-stack ()
  "Get the top of the stack without removing it"
  (when *stack* (car *stack*)))

(defun pop-stack ()
  "Remove the top of the stack"
  (if *stack* (pop *stack*)
      (error 'stack-underflow)))

(defun push-stack (item)
  "Push item on top of the stack"
  (when item (push item *stack*)))

(defun num-op-2 (op)
  "A number operation with 2 operands that come from the stack"
  (let ((a (pop-stack))
        (b (pop-stack)))
    (if (and a b)
        (push-stack (apply op (list a b)))
        (error 'stack-underflow))))

(defun dup ()
  "Duplicate the top element on the stack"
  (let ((a (peek-stack)))
    (if a (push-stack a)
        (error 'stack-underflow))))

(defun swap ()
  "Swap the two elements at the top of the stack"
  (let ((a (pop-stack))
        (b (pop-stack)))
    (if (and a b)
        (progn (push-stack a) (push-stack b))
        (error 'stack-underflow))))

;;; Definition of the Forth Dictionary

(defparameter *forth-dictionary*
  (let ((ht (make-hash-table :test 'equal)))
    (put-numop2 "+" #'+ ht)
    (put-numop2 "*" #'* ht)
    (put-numop2 "-" #'- ht)
    (put-numop2 "/" #'/ ht)
    (put-word "." (format t "~A~%" *stack*) ht)
    (put-word! "DROP" (pop-stack) "Drops 1 stack element." nil ht)
    (put-word! "DUP" (dup) "Duplicates a stack element." nil ht)
    (put-word! "SWAP" (swap) "Swaps top 2 stack elements." nil ht)
    ht)
    "Forth dictionary.
   Keys are of string type. Values are of forth-word type.
   The interpreter looks up a word in the dictionary and executes
   its function in :execute mode.
   In :compile mode, it just stores the function's address in the
   word being compiled.")

;;; INTERPRETER

(defun push-memory (item)
  (vector-push item *forth-memory*))

(defun exec-word (&optional word)
  "Execute the given word if not nil, otherwise the one on the stack."
  (apply (forth-word-function (or word (pop-stack))) nil))

(defun coerce-to-number (n)
  (if (numberp n) n
      (let ((n (ignore-errors (parse-integer n))))
        (or n (error :not-forth-value
                     (format nil "not a Forth value: ~A~%" n))))))

(deftype interpreter-state-t () '(member :exec :name-compile :name-quote))
(declaim (type (interpreter-state-t) *interpreter-state*))
(defparameter *interpreter-state* :exec)

(defparameter *compile-memory-start* 0)

(defun collect-words (start end)
  "Collect the word addresses that were left in memory"
  (loop for word-index from start to end
        collect (aref *forth-memory* word-index)))

(defun finish-compile ()
  (let ((start *compile-memory-start*)
        (end (fill-pointer *forth-memory*)))
    (if (> (- end start) 0)
        (let ((name (aref *forth-memory* start))
              (words (collect-words (1+ start) end)))
          (put-word name (dolist (w words)
                           (cond ((numberp w) (push-stack w))
                                 (t (exec-word w)))) *forth-dictionary*)))))

(defun process (words)
  "Forth interpreter."
  (flet ((do-immediate (fw w)
           (if fw (exec-word fw)
               (push-stack (coerce-to-number w))))
         (do-compile (fw w)
           (push-memory (or fw (coerce-to-number w)))))
    (dolist (word words)
      (cond
        ;; first, we need to check the state for intermediate work by the tokenizer
        ((eq *interpreter-state* :name-compile)
         (push-memory (string word))
         (setf *interpreter-state* :exec))
        ((eq *interpreter-state* :name-quote)
         (push-stack (gethash (string word) *forth-dictionary*))
         (setf *interpreter-state* :exec))
        ;; now, check for "code" words
        ((equal word "EXECUTE")
         (exec-word))
        ((equal word ":")
         (setf *forth-mode* :compile)
         (setf *interpreter-state* :name-compile)
         (setf *compile-memory-start* (fill-pointer *forth-memory*)))
        ((equal word ";")
         (finish-compile))
        ((equal word "'")
         (setf *interpreter-state* :name-quote))
        ;; finally, try to get words from the dictionary
        (t (let ((fw (gethash word *forth-dictionary*)))
             (if (eq *forth-mode* :immediate)
                 (do-immediate fw word)
                 (do-compile fw word))))))))

(defun process-from-lisp (&rest items)
  "Converts items coming directly from LISP to the types expected by `process`."
  (dolist (item items)
    (cond ((listp item)
           (apply #'process-from-lisp item))
          ((symbolp item) (process (list (symbol-name item))))
          ((numberp item) (process (list item)))
          ((stringp item) (process (list item)))
          (t (error :invalid-value (format nil "forth cannot handle value: ~A~%" item))))))
