(in-package :forth-cl)

(defmacro forth (&rest input)
  "The main Forth macro for writing Forth in Lisp.
   Pass a single string as argument, or Lisp symbols which will be
   converted to a string before being given to the interpreter."
  `(process-from-lisp ,@input))

(defparameter* (*stack* list) nil
  "The Forth data stack")

;; A few Forth interpreter variables

(defparameter* (*state* forth-mode-t) :immediate)

(defparameter* (*forth-dictionary* forth-dictionary-t) '()
    "Forth dictionary.
   Keys are of string type. Values are of forth-word type.
   The interpreter looks up a word in the dictionary and executes
   its function in :execute mode.
   In :compile mode, it just stores the function's address in the
   word being compiled.")

(defun* (current-word -> forth-word) ()
  "Get the current-word, i.e. the word at the top of the dictionary."
  (cdr (first *forth-dictionary*)))

(defvar symbols-package (find-package :forth-symbols))

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

(define-condition no-forth-word-defining (forth-error)
  ())

;;; Macros and functions to help define the "code words" written in Lisp

(defun* (to-key -> word-key) ((word (or string symbol)))
  "Ensure a WORD is stored correctly as a symbol in the symbols-package."
  (let ((str-name
          (etypecase word
            (string word)
            (symbol (symbol-name word)))))
    (intern (string-upcase str-name) symbols-package)))

(defun* (lookup-word -> ?forth-word) ((name (or string symbol)))
  (let ((entry (assoc (to-key name) *forth-dictionary* :test 'eq)))
    (and entry (cdr entry))))

(defmacro lambda-with-at-most-one-arg (args body)
  (let ((count (length args)))
    (ecase count
      (0 `(lambda (a) (declare (ignore a)) ,body))
      (1 `(lambda ,args ,body)))))

(defmacro put-word (word args body desc immediate)
  (let ((key (gensym))
        (fw (gensym)))
    `(let ((,key (to-key ,word))
           (,fw (make-instance
                 'forth-word
                 :fn (lambda-with-at-most-one-arg ,args ,body)
                 :description ,desc
                 :immediate ,immediate)))
       (push (cons ,key ,fw) *forth-dictionary*)
       ,fw)))

(defmacro put-numop2 (sym fun)
  `(put-word ,sym () (num-op-2 ,fun)
             (format nil "( N N -- N ) The ~A operation" ,sym) nil))

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
  (let ((b (pop-stack))
        (a (pop-stack)))
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

(defun* exec-word ((st input-stream-t) &optional ((word ?forth-word) nil))
  "Execute the given word if not nil, otherwise the one on the stack."
  (funcall (fn (or word (pop-stack))) st))

(defun* (create-word -> forth-word) ((st input-stream-t))
  (let ((name (read-word st)))
    (put-word name () (no-op-1 st) "" nil)))

;;; Definition of the fundamental Forth WORDs

(put-numop2 "+" #'+)
(put-numop2 "*" #'*)
(put-numop2 "-" #'-)
(put-numop2 "/" #'/)
(put-word "WORD" (st) (push-stack (read-word st))
          "( -- S ) Reads a word from stdin." T)
(put-word "KEY" (st) (push-stack (read-char st))
          "( -- C ) Reads a char from stdin." T)
(put-word "NUMBER" (st) (push-stack (read-number st))
          "( -- N ) Reads a number from stdin." T)
(put-word "FIND" () (push-stack (lookup-word (pop-stack)))
          "( S -- FW ) Finds a WORD definition in the dictionary." T)
(put-word "EXECUTE" (st) (exec-word st)
          "( i*x xt — j*x ) Executes a WORD." T)
(put-word ".S" () (format t "~A~%" *stack*)
          "( -- ) Prints the whole stack." nil)
(put-word "." () (format t "~A~%" (pop-stack))
          "( N -- ) Prints and drops the stack head." nil)
(put-word "DROP" () (pop-stack)
          "Drops 1 stack element." nil)
(put-word "DUP" () (dup)
          "Duplicates a stack element." nil)
(put-word "SWAP" () (swap)
          "Swaps top 2 stack elements." nil)
(put-word "IMMEDIATE" () (setf (mode (current-word)) :immediate)
          "Sets the mode of the current WORD to immediate." nil)
(put-word "CREATE" (st) (create-word st)
          "( -- FW ) Creates a FORTH WORD. The name is read from stdin." T)
(put-word "[" () (setf *state* :immediate)
          "Enter interpreter mode." T)
(put-word "]" () (setf *state* :compile)
          "Enter compilation mode." T)

;;; INTERPRETER

(defun push-memory (item)
  (let ((word (current-word)))
    (vector-push-extend item (get-data word))))

(defun* (coerce-to-number -> number) (n)
  (if (numberp n) n
      (let ((n (ignore-errors (parse-integer n))))
        (or n (error 'not-a-forth-value :value n)))))

(defun* (ensure-number -> number) (n)
  (if (numberp n) n
      (error 'not-a-number :word n)))

(defun* do-immediate ((stream input-stream-t)
                      &key ((word ?forth-word) nil) ((text ?string) nil))
  "Execute immediately the forth-word WORD, or push the TEXT to the stack as a number.
   Only one of WORD and TEXT should be non-null."
  (if word (exec-word stream word)
      (push-stack (coerce-to-number text))))

(defun* do-compile ((stream input-stream-t )
                    &key ((word ?forth-word) nil) ((text ?string) nil))
  "Compile the forth-word WORD, or TEXT as a number.
   Only one of WORD and TEXT should be non-null."
  (declare (ignore stream))
  (push-memory (or word (coerce-to-number text))))

(defun* interpret ((stream input-stream-t))
  "Forth interpreter."
  (flet ((do-word (word)
           (let ((fw (lookup-word word)))
             (if (or (eq *state* :immediate) (and fw (immediate fw)))
                 (do-immediate stream :word fw :text word)
                 (do-compile   stream :word fw :text word)))))
    ;; read words from the stream until end-of-file is reached
    (handler-case
        (loop for word = (read-word stream) then (read-word stream)
              do (do-word word))
      (end-of-file () :ok))))

(defun process-from-lisp (&rest items)
  "Converts items coming directly from LISP to the types expected by `interpret`."
  (let ((out (make-string-output-stream)))
    (labels ((to-strings
                 (objects)
               (loop for item in objects
                     do (format out "~A " (to-string item))))
             (to-string
                 (item)
               (typecase item
                 (list (to-strings item))
                 (symbol (symbol-name item))
                 (number (princ-to-string item))
                 (string item)
                 (character (string item))
                 (t (error 'not-a-forth-value :value item)))))
      (to-strings items)
      (interpret
       (make-string-input-stream (get-output-stream-string out))))))
