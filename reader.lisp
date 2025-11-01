(in-package :forth-cl)

(declaim (ftype (function (input-stream-t) string) read-word))
(defun read-word (stream)
  (let ((chars '()))
    ;; Skip leading whitespace
    (when (null (peek-char t stream nil nil))
      (error 'end-of-file :stream stream))
    ;; Read until whitespace, let EOF error propagate to caller
    (loop for char = (peek-char nil stream nil nil)
          while (and char (not (member char '(#\Space #\Tab #\Newline #\Return))))
          do (push (read-char stream) chars))
    (coerce (nreverse chars) 'string)))

(declaim (ftype (function (input-stream-t) number) read-number))
(defun read-number (stream)
  (let ((word (read-word stream)))
    (coerce (read-from-string word) 'number)))
