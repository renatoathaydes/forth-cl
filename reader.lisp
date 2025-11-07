(in-package :forth-cl)

(defun* (read-word -> string) ((stream input-stream-t))
  (let ((chars '()))
    ;; Skip leading whitespace
    (when (null (peek-char t stream nil nil))
      (error 'end-of-file :stream stream))
    ;; Read until whitespace, let EOF error propagate to caller
    (loop for char = (peek-char nil stream nil nil)
          while (and char (not (member char '(#\Space #\Tab #\Newline #\Return))))
          do (push (read-char stream) chars))
    (coerce (nreverse chars) 'string)))

(defun* (read-number -> number) ((stream input-stream-t))
  (let ((word (read-word stream)))
    (coerce (read-from-string word) 'number)))
