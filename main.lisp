(defmacro forth (&rest input)
  `(process-each '(,@input)))

(defun process-each (&rest items)
  (dolist (item items)
    (cond ((listp item)
           (apply #'process-each item))
          ((symbolp item) (process-word (symbol-name item)))
          ((numberp item) (process-word item))
          ((stringp item) (process-word item))
          (t (error (format nil "forth cannot handle value: ~A" item))))))

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
  (push item *stack*))

(defun dup ()
  (let ((a (peek-stack)))
    (when a (push-stack a))))

(defun swap ()
  (let ((a (pop-stack))
        (b (pop-stack)))
    (if (and a b)
        (push-stack a)
        (push-stack b))))

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
    ht))

(defun process(words)
  (dolist (word words)
    (process-word word)))

(defun process-word (word)
  (if (numberp word)
      (push-stack word)
      (let ((n (ignore-errors (parse-integer word))))
        (if n
            (push-stack n)
            (let ((op (gethash word *words*)))
              (if op
                  (apply op nil)
                  (push-stack word)))))))

(defun num-op-2 (op)
  (let ((a (pop-stack))
        (b (pop-stack)))
    (if (and a b)
        (push-stack (apply op (list a b)))
        (if b
            (format t "Too few operators!")
            (format t "The stack is empty!")))))

(defun run ()
  (dolist (item (cdr sb-ext:*posix-argv*))
    (process-word item)))
