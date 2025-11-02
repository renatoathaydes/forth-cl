(asdf:defsystem "forth-cl"
  :description "A FORTH embedded in Common Lisp"
  :version "0.1.0"
  :author "Renato Athaydes"
  :license "GPL"
  :components ((:file "package")
               (:file "types" :depends-on ("package"))
               (:file "reader" :depends-on ("package" "types"))
               (:file "main" :depends-on ("package" "types" "reader")))
  :in-order-to ((asdf:test-op (asdf:test-op "forth-cl/tests"))))

(asdf:defsystem "forth-cl/tests"
  :depends-on ("forth-cl" "parachute")
  :components ((:module "test"
                :components ((:file "package")
                             (:file "basic" :depends-on ("package")))))
  :perform (asdf:test-op (o c) (uiop:symbol-call :parachute :test :forth-cl/tests)))
