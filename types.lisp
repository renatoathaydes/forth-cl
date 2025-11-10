(in-package :forth-cl)

(deftype forth-mode-t () `(member :immediate :compile))

(deftype input-stream-t ()
  "Type satisfying INPUT-STREAM-P."
  `(satisfies input-stream-p))

(deftype forth-function-t ()
  "Type of the function of a FORTH-WORD."
  '(function (input-stream-t)))

(deftype word-key ()
  "Type of the keys in cells of *FORTH-DICTIONARY* ALIST."
  `(satisfies symbolp))

(defun alist-of-forth-words-p (list)
  "Return t if LIST is non nil and contains only FORTH-WORDs."
  (and (listp list)
       (every #'(lambda (a)
                  (and (consp a)
                       (let ((k (car a))
                             (v (cdr a)))
                         (and (typep k 'word-key)
                              (typep v 'forth-word)))))
              list)))

(deftype forth-dictionary-t ()
  "Type of the *FORTH-DICTIONARY*"
  `(satisfies alist-of-forth-words-p))

(defun no-op-1 (a) (declare (ignore a)))

(defclass forth-word ()
  ((function     :type forth-function-t
                 :accessor fn
                 :initarg :fn
                 :initform #'no-op-1
                 :documentation
                 "similar to the 'code field' in traditional Forth.")
   (description  :type string
                 :accessor description
                 :initarg :description
                 :initform ""
                 :documentation
                 "a description of this word.")
   (immediate    :type boolean
                 :accessor immediate
                 :initarg :immediate
                 :initform nil
                 :documentation
                 "whether this is an immediate word.")
   (smudge       :type boolean
                 :accessor smudge
                 :initarg :smudge
                 :initform T
                 :documentation
                 "whether this word is hidden. This flag starts as true,
                  but is expected to be set to false when its definition is finished.")
   (data         :type array
                 :accessor get-data
                 :initarg :data
                 :initform (make-array 0 :adjustable T :fill-pointer T)
                 :documentation
                 "The data managed by this word. Equivalent to the 'parameter field'."))
  (:documentation
   "A Forth word definition.
    All Forth definitions are stored using this class.
    Unlike in traditional Forth, a FORTH-WORD does not keep a link to the next one."))

(deftype ?forth-word () '(or null forth-word))

(deftype ?string () '(or null string))
