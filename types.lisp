(in-package :forth-cl)

(deftype input-stream-t ()
  `(satisfies input-stream-p))

;; execute modes are:
;;    - immediate: fetch and immediately execute words.
;;    - compile: fetch word, leave it in memory.
(deftype forth-mode-t () '(member :immediate :compile))
