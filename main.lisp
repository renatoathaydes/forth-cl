(in-package :forth-cl)

(defmacro forth (&rest input)
  "The main Forth macro for writing Forth in Lisp.
   Pass a single string as argument, or Lisp symbols which will be
   converted to a string before being given to the interpreter."
  (if (and (= (length input) 1) (stringp (first input)))
      (let ((s (first input)))
        `(interpret (make-string-input-stream ,s)))
      `(process-from-lisp '(,@input))))

(defparameter *stack* nil
  "The Forth data stack")

;; A few Forth interpreter data structures

(defstruct forth-variable value)

(defstruct forth-word
  (function (lambda ()) :type function)
  (immediate nil    :type boolean)
  (description ""   :type string))

(defparameter *forth-memory*
  (make-array 64 :fill-pointer 0 :adjustable t :initial-element nil))

(declaim (type (forth-mode-t) *forth-mode*))
(defparameter *forth-mode* :immediate)

;;; Error conditions

(define-condition forth-error (error) ())

(define-condition stack-underflow (forth-error) ()
  (:documentation "Not enough elements on the stack to perform operation"))

(define-condition not-a-number (forth-error)
  ((word :initarg :word :reader not-a-number-word))
  (:documentation "The element at the top of the stack was not a number"))

(define-condition not-a-forth-value (forth-error)
  ((value :initarg :value :reader not-a-forth-value-value))
  (:documentation "Entered value is not a Forth word nor a number"))

(define-condition undefined-word (forth-error)
  ((word :initarg :word :reader undefined-word-word)))

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
        (push-stack (funcall op a b))
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
  (let ((ht (make-hash-table :test 'equalp)))
    (put-numop2 "+" #'+ ht)
    (put-numop2 "*" #'* ht)
    (put-numop2 "-" #'- ht)
    (put-numop2 "/" #'/ ht)
    (put-word ".S" (format t "~A~%" *stack*) ht)
    (put-word "." (format t "~A~%" (pop-stack)) ht)
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
  (vector-push-extend item *forth-memory*))

(defun exec-word (&optional word)
  "Execute the given word if not nil, otherwise the one on the stack."
  (apply (forth-word-function (or word (pop-stack))) nil))

(defun coerce-to-number (n)
  (if (numberp n) n
      (let ((n (ignore-errors (parse-integer n))))
        (or n (error 'not-a-forth-value :value n)))))

(defun ensure-number (n)
  (if (numberp n) n
      (error 'not-a-number :word n)))

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

(defun do-immediate (&key word text)
  (if word (exec-word word)
      (push-stack (coerce-to-number text))))

(defun do-compile (&key word text)
  (push-memory (or word (coerce-to-number text))))

(declaim (ftype (function (input-stream-t)) interpret))
(defun interpret (stream)
  "Forth interpreter."
  (flet ((do-word (word)
           (cond
             ;; fundamental code words
             ((equalp word "WORD")
              (push-stack (read-word stream)))
             ((equalp word "KEY")
              (push-stack (read-char stream)))
             ((equalp word "NUMBER")
              (push-stack (read-number stream)))
             ((equalp word "FIND")
              (push-stack (gethash (pop-stack) *forth-dictionary*)))
             ((equalp word "HERE")
              (aref *forth-memory* (fill-pointer *forth-memory*)))
             ((equalp word ",")
              (push-memory (pop-stack)))
             ((equalp word "EXECUTE")
              (exec-word))
             ((equalp word ";")
              (finish-compile))
             ;; finally, try to use words from the dictionary
             (t (let ((fw (gethash word *forth-dictionary*)))
                  (if (eq *forth-mode* :immediate)
                      (do-immediate :word fw :text word)
                      (do-compile :word fw :text word)))))))
    ;; read words from the stream until end-of-file is reached
    (handler-case
        (loop for word = (read-word stream) then (read-word stream)
              do (do-word word))
      (end-of-file () :ok))))

(defun process-from-lisp (&rest items)
  "Converts items coming directly from LISP to the types expected by `interpret`."
  (dolist (item items)
    (cond ((listp item)
           (apply #'process-from-lisp item))
          ((symbolp item) (interpret (list (symbol-name item))))
          ((numberp item) (interpret (list item)))
          ((stringp item) (interpret (list item)))
          (t (error 'not-a-forth-value :value item)))))
