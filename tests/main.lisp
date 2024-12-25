(defpackage nesoteric/tests/main
  (:use :cl
        :nesoteric
        :nesoteric/cpu
        :rove)
  (:local-nicknames (#:cpu #:nesoteric/cpu)))
(in-package :nesoteric/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :nesoteric)' in your Lisp.

#+or
(rove:run-suite *package*)
