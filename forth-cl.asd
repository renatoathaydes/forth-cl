(asdf:defsystem :forth-cl
  :components ((:file "package")
               (:file "types" :depends-on ("package"))
               (:file "reader" :depends-on ("package" "types"))
               (:file "main" :depends-on ("package" "types" "reader"))))
