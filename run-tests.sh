#! /bin/sh

sbcl --script /dev/stdin <<'EOF'
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(setq *backtrace-frame-count* 8)
(require "quicklisp")
(require "asdf")
(load "forth-cl.asd")
(ql:quickload "forth-cl")
(asdf:test-system "forth-cl")
EOF
