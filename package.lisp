(defpackage :forth-cl
  (:use :cl :defstar)
  (:export
   :forth
   :forth-word
   :forth-error
   :not-a-number :not-a-number-word
   :not-a-forth-value :not-a-forth-value-value
   :undefined-word :undefined-word-word))
