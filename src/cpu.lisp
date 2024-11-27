(uiop:define-package nesoteric/cpu
  (:use #:cl))
(in-package #:nesoteric/cpu)

(defstruct cpu
  (accumulator     0 :type unsigned-byte)
  (x-register      0 :type unsigned-byte)
  (y-register      0 :type unsigned-byte)
  (program-counter 0 :type (unsigned-byte 16))
  (stack-pointer   0 :type unsigned-byte)
  (status-register 0 :type unsigned-byte))

(make-cpu)

(defparameter *tcpu* (make-cpu))

(defun get-carry-flag ())
(defun get-zero-flag ())
(defun get-interrupt-flag ())
(defun get-decimal-flag ())
(defun get-b-flag ())
(defun get-overflow-flag ())
(defun get-negative-flag ())

;; https://www.pagetable.com/?p=410
;; http://visual6502.org/JSSim/expert.html
;; https://fms.komkon.org/EMUL8/NES.html#LABM

;; https://www.nesdev.org/wiki/CPU_power_up_state
;; | Register |      At Power | After Reset |
;; | A, X, Y  |             0 | unchanged   |
;; | PC       |         $FFFC | $FFFC       |
;; | S        | $00 - 3 = $FD | S -= 3      |
;; | C        |             0 | unchanged   |
;; | Z        |             0 | unchanged   |
;; | I        |             1 | 1           |
;; | D        |             0 | unchanged   |
;; | V        |             0 | unchanged   |
;; | N        |             0 | unchanged   |
(defun power-on ())
(defun reset ())
