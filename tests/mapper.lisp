
(defpackage nesoteric.tests.mapper
  (:use :cl
        :nesoteric
        :nesoteric.cpu
        :nesoteric.memory
        :nesoteric.mapper
        :nesoteric.bits-n-bytes
        :rove)
  ;; (:local-nicknames (#:cpu #:nesoteric.cpu))
  )
(in-package :nesoteric.tests.mapper)

#+or
(rove:run-suite *package*)

#+or
(rove:run-test 'test-mapper-0)

;; (defparameter *cpu* (cpu::make-cpu))
;; (defparameter *mem* (make-memory))

(defparameter *mapper* (make-mapper))

;; (defun setup-test-memory ()
;;   (setf *cpu* (cpu::make-cpu)
;;         *mem* (make-memory)
;;         *mapper* (make-mapper)))

(defun setup-mapper ()
  (setf *mapper* (make-mapper)
        (mapper-cart-ram *mapper*) (make-array #x2000
                                               :element-type
                                               'u8
                                               :initial-element 0)))

(deftest test-mapper-0
  (testing "read and write expansion RAM"
    (setup-mapper)
    (let ((passed t))
      (dotimes (i #x2000)               ; RAM size
        (let ((r (random 256))          ; Random Byte
              (addr (+ i #x6000)))      ; RAM start address
          ;; (format t "~a ~x~%" i addr)
          (nesoteric.mapper::mapper-0-writer *mapper* addr r)
          (when (/= r (nesoteric.mapper::mapper-0-reader *mapper* 1 addr))
            (setf passed nil)
            (return))))
      (ok passed))))
