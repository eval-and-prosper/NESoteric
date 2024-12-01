(uiop:define-package nesoteric/cpu
  (:use #:cl))

(in-package #:nesoteric/cpu)

;; TODO not sure if we need to care about the unused status flag
;; TODO need to remember NES is little endian

(deftype u8  () '(unsigned-byte 8))
(deftype s8  () '(signed-byte 8))
(deftype u16 () '(unsigned-byte 16))
(deftype s16 () '(signed-byte 16))


(defstruct cpu
  (accumulator     0 :type u8)
  (x-register      0 :type u8)
  (y-register      0 :type u8)
  (program-counter 0 :type u16)
  (stack-pointer   0 :type u8)
  (status-register 0 :type u8)
  (ram (make-array #x800 :element-type 'u8 :initial-element 0)
   :type (simple-array u8 (#x800))))

;; (defun get-acc (&optional (cpu *default-cpu*))
;;   (cpu-accumulator cpu))
;; (defun get-x (&optional (cpu *default-cpu*))
;;   (cpu-x-register cpu))
;; (defun get-y (&optional (cpu *default-cpu*))
;;   (cpu-y-register cpu))

(defparameter *default-cpu* (make-cpu))

(defun set-acc (val &optional (cpu *default-cpu*))
  (setf (cpu-accumulator cpu) val))
(defun set-x (val &optional (cpu *default-cpu*))
  (setf (cpu-x-register cpu) val))
(defun set-y (val &optional (cpu *default-cpu*))
  (setf (cpu-y-register cpu) val))


(defun get-bit (byte position)
  "Retrieve the bit at POSITION (0-7) in BYTE."
  (declare (type (unsigned-byte 8) byte)
           (type (integer 0 7) position)) ; Ensure position is in range
  (ldb (byte 1 position) byte))

(defun set-bit (byte position value)
  "Set the bit at POSITION (0-7) in BYTE to VALUE (0 or 1)."
  (declare (type (unsigned-byte 8) byte)
           (type (integer 0 7) position)
           (type (integer 0 1) value)) ; Ensure value is 0 or 1
  (if (zerop value)
      (logand byte (lognot (ash 1 position))) ; Clear the bit
      (logior byte (ash 1 position))))       ; Set the bit

(defun print-bits (n size)
  (format t (format nil "~~~D,'0B" size) (ldb (byte size 0) n))
  (values))

;; TODO not sure but should probably add unused 1 bit
(defconstant +status-flags+
  '((:carry     . 0)
    (:zero      . 1)
    (:interrupt . 2)
    (:decimal   . 3)
    (:b         . 4)
    (:overflow  . 6)
    (:negative  . 7)))

(defun get-flag (flag-name &optional (cpu *default-cpu*))
  "Get the value of the keyword CPU status flag in +status-flags+"
  (let ((bit-position (cdr (assoc flag-name +status-flags+))))
    (unless bit-position
      (error "Invalid flag name: ~A" flag-name))
    (get-bit (cpu-status-register cpu) bit-position)))

(defun set-flag (flag-name value &optional (cpu *default-cpu*))
  "Set the value of the keyword CPU status flag in +status-flags+"
  (let ((bit-position (cdr (assoc flag-name +status-flags+))))
    (unless bit-position
      (error "Invalid flag name: ~A" flag-name))
    (setf (cpu-status-register cpu)
          (set-bit (cpu-status-register cpu) bit-position value))))

(defun print-flags ()
  (dolist (i +status-flags+)
    (format t "~a ~a~%" (get-flag (car i)) (car i))))

;; https://www.pagetable.com/?p=410
;; http://visual6502.org/JSSim/expert.html
;; https://fms.komkon.org/EMUL8/NES.html#LABM
;; https://masswerk.at/6502/6502_instruction_set.html

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
;; TODO not sure but the missing bit should probably always be on
(defun power-on (&optional (cpu *default-cpu*))
  (setf (cpu-accumulator cpu) 0)
  (setf (cpu-x-register cpu) 0)
  (setf (cpu-y-register cpu) 0)
  (setf (cpu-program-counter cpu) #xFFFC)
  (setf (cpu-stack-pointer cpu) #xFD)
  (setf (cpu-status-register cpu) 4)
  (setf (cpu-ram cpu) (make-array #x800
                                  :element-type 'u8
                                  :initial-element 0))
  t)

;; TODO figure out exactly how reset works
(defun reset ()
  (error "reset cpu not implemented"))

(defparameter *op-codes* nil)

;; adc add with carry
;; A = A + memory + C
;; | Addressing mode | Opcode | Bytes |                Cycles |
;; |-----------------+--------+-------+-----------------------|
;; | #Immediate      | $69    |     2 |                     2 |
;; | Zero Page       | $65    |     2 |                     3 |
;; | Zero Page,X     | $75    |     2 |                     4 |
;; | Absolute        | $6D    |     3 |                     4 |
;; | Absolute,X      | $7D    |     3 | 4 (5 if page crossed) |
;; | Absolute,Y      | $79    |     3 | 4 (5 if page crossed) |
;; | (Indirect,X)    | $61    |     2 |                     6 |
;; | (Indirect),Y    | $71    |     2 | 5 (6 if page crossed) |

;; (:immediate   #x69 2 2)
;; (:zero-page   #x65 2 3)
;; (:zero-page-x #x75 2 4)
;; (:absolute    #x6D 3 4)
;; (:absolute-x  #x7D 3 4 :page-cross 1)
;; (:absolute-y  #x79 3 4 :page-cross 1)
;; (:indirect-x  #x61 2 6)
;; (:indirect-y  #x71 2 5 :page-cross 1)


(defun addc (mem)
  ;; (declare (optimize (speed 3) (safety 0)))
  (declare (type u8 mem))
  (let* ((A (cpu-accumulator *default-cpu*))
         (carry (get-flag :carry))
         (res (+ A mem carry)))
    (declare (type u8 A)
             (type bit carry))

    (set-flag :carry (if (> res #xFF) 1 0))
    (setf res (logand res #xFF))

    (set-flag :zero (if (zerop res) 1 0))

    ;; (result ^ A) & (result ^ memory) & $80
    ;; Formula detects overflow by checking sign changes:
    ;; XOR sets bit 7 to 1 when operands have different signs
    ;; ANDing the XORs will be 1 in bit 7 only if result differs from both inputs
    ;; Final & $80 isolates bit 7 (the sign bit)
    ;; We then shift to extract the bit and set the flag with it
    (set-flag :overflow
              (ash (logand (logxor res A)
                           (logxor res mem)
                           #x80)
                   -7))
    (set-flag :negative (get-bit res 7))
    (set-acc res)))

;; ==============================================================================
;; Memory
;; ==============================================================================
;; | Address range | Size  | Device                                         |
;; |---------------+-------+------------------------------------------------|
;; | $0000–$07FF   | $0800 | 2 KB internal RAM                              |
;; | $0800–$0FFF   | $0800 | Mirrors of $0000–$07FF                         |
;; | $1000–$17FF   | $0800 |                                                |
;; | $1800–$1FFF   | $0800 |                                                |
;; | $2000–$2007   | $0008 | NES PPU registers                              |
;; | $2008–$3FFF   | $1FF8 | Mirrors of $2000–$2007 (repeats every 8 bytes) |
;; | $4000–$4017   | $0018 | NES APU and I/O registers                      |
;; | $4018–$401F   | $0008 | APU and I/O normally disabled.                 |
;; | $4020–$FFFF   | $BFE0 | Unmapped. Available for cartridge use.         |

(defun print-memory ()
  (dotimes (i #x0800)
    (if (= 0 (mod i 38)) (format t "~%"))
    (format t "~a " (read-memory i))))

(defun read-memory (address)
  (declare (type u16 address))
  (cond
    ((< address #x2000)
     (aref (cpu-ram *default-cpu*) (logand #x07FF address)))
    ((< address #x4000)
     (error "Reading PPU registers not yet implemented."))
    ((< address #x4020)
     (error "Reading APU registers not yet implemented."))
    ;; don't check beyond #FFFF as address will throw error if not u16
    ((<= address #xFFFF)
     (error "Reading cartridge not yet implemented."))))

(defun write-memory (address value)
  (declare
   (type u16 address)
   (type u8 value))
  (cond
    ((< address #x2000)
     (setf (aref (cpu-ram *default-cpu*)
                 (logand #x07FF address)) value))
    ((< address #x4000)
     (error "Writing PPU registers not yet implemented."))
    ((< address #x4020)
     (error "Writing APU registers not yet implemented."))
    ;; don't check beyond #FFFF as address will throw error if not u16
    ((<= address #xFFFF)
     (error "Writing cartridge not yet implemented."))))
