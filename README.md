# Forth-CL

> This project is in early development!

A Forth embedded in Common Lisp.

Example in the CL REPL:

```lisp
(forth "1 2 + .") ;; => 3
```

Example in the terminal:

```shell
$ forth-cl
1 2 3 + .
(5 1)
:OK
```

## Loading forth-cl in SLIME

If forth-cl is not in your load path, open `forth-cl.asd` in emacs then run
`slime-load-file`.

Now, on the REPL, run:

```lisp
(asdf:make "forth-cl")
```

Now, the `forth` macro should be available:

```lisp
CL-USER> (forth-cl:forth "2 3 + .S")
(5)
:OK
```
