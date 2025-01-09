;; repl should be started in root project directory
(let ((p (uiop:getcwd)))
  (when (null (member p asdf:*central-registry*))
    (format t "Adding project to ASDF registry~%")
    (push p asdf:*central-registry*)
    (format t "Loading NESoteric system~%")
    (asdf:load-system :nesoteric)))

(directory ".")

(asdf:load-system :nesoteric)

(asdf:clear-system :nesoteric)
