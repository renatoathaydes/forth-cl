(in-package :forth-cl)

(deftype input-stream-t ()
  "Type satisfying INPUT-STREAM-P"
  `(satisfies input-stream-p))

(deftype forth-function-t ()
  "Type of the function of a FORTH-WORD"
  '(function (input-stream-t)))

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
                 :initform #()
                 :documentation
                 "The data managed by this word. Equivalent to the 'parameter field'."))
  (:documentation
   "A Forth word definition.
    All Forth definitions are stored using this class.
    Unlike in traditional Forth, a FORTH-WORD does not keep a link to the next one."))

;; execute modes are:
;;    - immediate: fetch and immediately execute words.
;;    - compile: fetch word, leave it in memory.
(deftype forth-mode-t () `(member :immediate :compile))

(deftype ?forth-word () '(or null forth-word))

(deftype ?string '(or null string))
