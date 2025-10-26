# Forth-CL

> This project is in early development!

A Forth embedded in Common Lisp.

Example in the CL REPL:

```lisp
(forth 1 2 + :.) ;; => 3
```

Notice that CL interprets `.` specially, so it must be _escaped_ as above
with `:.`.

Example in the terminal:

```shell
$ forth-cl 1 2 3 + .
(5 1)
```
