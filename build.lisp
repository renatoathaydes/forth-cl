(load "main.lisp")

(sb-ext:save-lisp-and-die
 #P"forth-cl"
 :toplevel #'run
 :executable t
 :compression t)
