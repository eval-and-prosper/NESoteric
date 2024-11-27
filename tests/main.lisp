(defpackage nesoteric/tests/main
  (:use :cl
        :nesoteric
        :rove))
(in-package :nesoteric/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :nesoteric)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
