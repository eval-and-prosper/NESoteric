(uiop:define-package nesoteric/cpu
  (:use #:cl
        #:alexandria)
  (:import-from :alexandria
                :define-constant))

(in-package #:nesoteric/cpu)

;; TODO not sure if we need to care about the unused status flag
;; TODO need to remember NES is little endian
;; TODO should probably make a flags macro that sets them based on result
;; with only the flags that were specified to the macro

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

(defun cpu-step (&optional (cpu *default-cpu*))
  (let* ((opcode (read-memory (cpu-program-counter cpu)))
         (inst (aref *instructions* opcode)))
    (if inst
        (progn
          (inc-pc)
          (funcall inst))
        (format t "Invalid Opcode~%"))))

(defun print-cpu (&optional (cpu *default-cpu*))
  (fresh-line)
  (format t "ACC ~A~%" (cpu-accumulator cpu))
  (format t "X   ~A~%" (cpu-x-register cpu))
  (format t "Y   ~A~%" (cpu-y-register cpu))
  (format t "PC  ~A~%" (cpu-program-counter cpu))
  (format t "SP  ~A~%" (cpu-stack-pointer cpu))
  (format t "ST  ~b~%" (cpu-status-register cpu)))

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
(defun set-pc (val &optional (cpu *default-cpu*))
  (setf (cpu-program-counter cpu) val))

(defun inc-pc (&optional (amount 1) (cpu *default-cpu*))
  (setf (cpu-program-counter cpu) (+ amount (cpu-program-counter cpu))))

(defun dec-sp (&optional (amount 1) (cpu *default-cpu*))
 (setf (cpu-stack-pointer *default-cpu*)
        (logand #xFF (- (cpu-stack-pointer cpu) amount))))

(defun inc-sp (&optional (amount 1) (cpu *default-cpu*))
 (setf (cpu-stack-pointer *default-cpu*)
        (logand #xFF (+ (cpu-stack-pointer cpu) amount))))


(defun get-bit (byte position)
  "Retrieve the bit at POSITION (0-7) in BYTE."
  (declare (type (unsigned-byte 8) byte)
           (type (integer 0 7) position)) ; Ensure position is in range
  (ldb (byte 1 position) byte))

(defun set-bit (byte position value)
  "Set the bit at POSITION (0-7) in BYTE to VALUE (0 or 1)."
  (declare (type (unsigned-byte 8) byte)
           (type (integer 0 7) position)
           (type (integer 0 1) value)
           (optimize (debug 0) (speed 3))) ; Ensure value is 0 or 1
  (if (zerop value)
      (logand byte (lognot (ash 1 position))) ; Clear the bit
      (logior byte (ash 1 position))))       ; Set the bit

(defun u8-to-s8 (byte)
  (if (>= byte 128)
      (- byte 256)
      (the s8 byte)))

(defun print-bits (n size)
  (format t (format nil "~~~D,'0B" size) (ldb (byte size 0) n))
  (values))

;; TODO not sure but should probably add unused 1 bit
(define-constant +status-flags+
    '((:carry     . 0)
      (:zero      . 1)
      (:interrupt . 2)
      (:decimal   . 3)
      (:b         . 4)
      (:overflow  . 6)
      (:negative  . 7))
  :test #'equal)

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

(defun print-memory (&optional (start 0) (count 0))
  (if (> count 0)
      (format t "~a~%" (subseq (cpu-ram *default-cpu*) start (+ start count)))
      (dotimes (i #x0800)
        (if (= 0 (mod i 38)) (format t "~%"))
        (format t "~a " (read-memory i))
        (if (= i #x07FF)
            (format t "~%")))))

;; TODO return type?
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

;; TODO replace redundant code with this function
(defun read-memory-u16 (address)
  (let ((mem1 (read-memory address))
        (mem2 (read-memory (1+ address))))
    (+ (ash mem2 8) mem1)))

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

;; ============================================================================
;; Addressing
;; ============================================================================

;; TODO need to test addressing modes
;; all of these assume pc already points to the first byte
(defun implied() nil)

(defun immediate ()
  (let* ((pc (cpu-program-counter *default-cpu*))
         ;; (mem (read-memory pc))
         )
    (inc-pc)
    ;; mem
    pc))

(defun absolute ()
  (let* ((pc (cpu-program-counter *default-cpu*))
         (b1 (read-memory pc))
         (b2 (read-memory (1+ pc)))
         ;; (mem (read-memory (+ (ash b2 8) b1)))
         (addr (+ (ash b2 8) b1)))
    (inc-pc 2)
    ;; mem
    addr))

(defun zero-page ()
  (let* ((pc (cpu-program-counter *default-cpu*))
         (addr (read-memory pc))
         ;; (mem (read-memory addr))
         )
    (inc-pc)
    ;; mem
    addr))

;; TODO check overflow
(defun absolute-x ()
  (let* ((pc (cpu-program-counter *default-cpu*))
         (b1 (read-memory pc))
         (b2 (read-memory (1+ pc)))
         ;; (mem (read-memory (+ (ash b2 8)
         ;;                      b1
         ;;                      (cpu-x-register *default-cpu*))))
         (addr (+ (ash b2 8) b1 (cpu-x-register *default-cpu*)))
         )
    (inc-pc 2)
    ;; mem
    addr))

;; TODO check overflow
(defun absolute-y ()
  (let* ((pc (cpu-program-counter *default-cpu*))
         (b1 (read-memory pc))
         (b2 (read-memory (1+ pc)))
         ;; (mem (read-memory (+ (ash b2 8)
         ;;                      b1
         ;;                      (cpu-y-register *default-cpu*))))
         (addr (+ (ash b2 8) b1 (cpu-y-register *default-cpu*)))
         )
    (inc-pc 2)
    ;; mem
    addr))

;; zero-page,x
(defun zero-page-x ()
  (let* ((pc (cpu-program-counter *default-cpu*))
         (zp (read-memory pc))
         ;; (mem (read-memory (logand #xFF (+ addr (cpu-x-register *default-cpu*)))))
         (addr (logand #xFF (+ zp (cpu-x-register *default-cpu*)))))
    (inc-pc)
    ;; mem
    addr))

;; zero-page,y
(defun zero-page-y ()
  (let* ((pc (cpu-program-counter *default-cpu*))
         (zp (read-memory pc))
         ;; (mem (read-memory (logand #xFF (+ addr (cpu-y-register *default-cpu*)))))
         (addr (logand #xFF (+ zp (cpu-y-register *default-cpu*)))))
    (inc-pc)
    ;; mem
    addr))

(defun indirect ()
  (let* ((pc (cpu-program-counter *default-cpu*))
         (b1 (read-memory pc))
         (b2 (read-memory (1+ pc)))
         (addr (+ (ash b2 8) b1))
         (mem1 (read-memory addr))
         (mem2 (read-memory (1+ addr))))
    (inc-pc 2)
    (+ (ash mem2 8) mem1)))

;; (zero-page,x)
;; TODO no overflow?
(defun pre-indexed-indirect ()
  (declare (optimize (debug 3)))
  (let* ((pc (cpu-program-counter *default-cpu*))
         (op (logand #xFF (+ (read-memory pc) (cpu-x-register *default-cpu*))))
         (b1 (read-memory op))
         (b2 (read-memory (logand #xFF (1+ op))))
         (addr (+ (ash b2 8) b1)))
    (inc-pc)
    ;; (read-memory eff-addr)
    addr))

;; (zero-page),y
;; TODO check overflow
(defun post-indexed-indirect ()
  (let* ((pc (cpu-program-counter *default-cpu*))
         (op (read-memory pc))
         (b1 (read-memory op))
         (b2 (read-memory (1+ op)))
         (addr (+ (ash b2 8) b1 (cpu-y-register *default-cpu*))))
    (inc-pc)
    ;; (read-memory eff-addr)
    addr))

;; TODO I don't think this is right we aren't adding this
;; to the the pc, but I'm not sure when you add it to the pc
;; before or after?
;; not sure how to fix this up for returning addresses like the others
(defun relative ()
  (let* ((pc (cpu-program-counter *default-cpu*))
         (b (read-memory pc))
         )
    (inc-pc)
    b
    ;; pc
    ))

;; TODO whoops need addressing modes for accumulator
(defun accumulator ())


;; Should be a constant but it looks like that's impossible
(defparameter +addressing-modes+
  `((:implied nil)
    (:accumulator nil)
    (:immediate ,#'immediate)
    (:absolute ,#'absolute)
    (:zero-page ,#'zero-page)
    (:absolute-x ,#'absolute-x)
    (:absolute-y ,#'absolute-y)
    (:zero-page-x ,#'zero-page-x)
    (:zero-page-y ,#'zero-page-y)
    (:indirect ,#'indirect)
    (:pre-indexed-indirect ,#'pre-indexed-indirect)
    (:post-indexed-indirect ,#'post-indexed-indirect)))

;; =============================================================================
;; Instructions
;; =============================================================================

(defparameter *instructions* (make-array #x100
                                         ;; :element-type 'function
                                         :initial-element nil))

#+or
(setf *instructions* (make-array #x100 :initial-element nil))

(defun update-z-flag (value)
  (set-flag :zero (if (zerop value) 1 0)))

(defun update-n-flag (value)
  (set-flag :negative (get-bit value 7)))

(defun update-zn-flags (value)
  (set-flag :zero (if (zerop value) 1 0))
  (set-flag :negative (get-bit value 7)))

(defun make-instruction (inst-fun inst-list)
  (dolist (inst inst-list)
    (let* (;; (inst-fun #'addc)
           (addr-mode (second (assoc (first inst) +addressing-modes+)))
           (opcode (second inst)))
      (when (aref *instructions* opcode)
        (format t "Redefining instruction: ~X" opcode))
      (setf (aref *instructions* opcode)
            (if (eql :implied (first inst))
                (lambda ()
                  (funcall inst-fun))
                (lambda ()
                  (let ((byte (funcall addr-mode)))
                    (funcall inst-fun byte))))))))

;; adc add with carry
;; A = A + memory + C
(defun adc (address)
  ;; (declare (type u8 mem))
  (let* ((A (cpu-accumulator *default-cpu*))
         (carry (get-flag :carry))
         (mem (read-memory address))
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

(make-instruction
 #'adc
 '((:immediate   #x69 2 2)
   (:zero-page   #x65 2 3)
   (:zero-page-x #x75 2 4)
   (:absolute    #x6D 3 4)
   (:absolute-x  #x7D 3 4 :page-cross 1)
   (:absolute-y  #x79 3 4 :page-cross 1)
   (:indirect-x  #x61 2 6)
   (:indirect-y  #x71 2 5 :page-cross 1)))

;; | Addressing mode | Opcode | Bytes |                Cycles |
;; |-----------------+--------+-------+-----------------------|
;; | Immediate       | $29    |     2 |                     2 |
;; | Zero Page       | $25    |     2 |                     3 |
;; | Zero Page,X     | $35    |     2 |                     4 |
;; | Absolute        | $2D    |     3 |                     4 |
;; | Absolute,X      | $3D    |     3 | 4 (5 if page crossed) |
;; | Absolute,Y      | $39    |     3 | 4 (5 if page crossed) |
;; | (Indirect,X)    | $21    |     2 |                     6 |
;; | (Indirect),Y    | $31    |     2 |  5 (6 if page crossed |
;; A = A & Memory
;; Z result == 0
;; N result bit 7
(defun inst-and (address)
  ;; (declare (type u8 mem))
  (let* ((mem (read-memory address))
        (result (logand mem (cpu-accumulator *default-cpu*))))
    ;; (set-flag :zero (if (zerop result) 1 0))
    ;; (set-flag :negative (get-bit result 7))
    (update-zn-flags result)
    (set-acc result)))

(make-instruction
 #'inst-and
 '((:immediate   #x29 2 2)
   (:zero-page   #x25 2 3)
   (:zero-page-x #x35 2 4)
   (:absolute    #x2D 3 4)
   (:absolute-x  #x3D 3 4 :page-cross 1)
   (:absolute-y  #x39 3 4 :page-cross 1)
   (:indirect-x  #x21 2 6)
   (:indirect-y  #x31 2 5 :page-cross 1)))

(defun asl (address) address)
(defun bcc (address) address)
(defun bcs (address) address)
(defun beq (address) address)
(defun inst-bit (address) address)
(defun bmi (address) address)
(defun bne (address) address)
(defun bpl (address) address)
(defun brk (address) address)
(defun bvc (address) address)
(defun bvs (address) address)

(defun clc ()
  (set-flag :carry 0))

(make-instruction
 #'clc
 '((:implied #x18 1 2)))

(defun cld ()
  (set-flag :decimal 0))

(make-instruction
 #'cld
 '((:implied #xD8 1 2)))

(defun cli ()
  (set-flag :interrupt 0))

(make-instruction
 #'cli
 '((:implied #x58 1 2)))

(defun clv ()
  (set-flag :overflow 0))

(make-instruction
 #'clv
 '((:implied #xB8 1 2)))

(defun cmp (address) address)
(defun cpx (address) address)
(defun cpy (address) address)

(defun dec (address) address)

(defun dex ()
  (setf (cpu-x-register *default-cpu*) (logand #xFF (- (cpu-x-register *default-cpu*) 1)))
  (update-zn-flags (cpu-x-register *default-cpu*)))

(make-instruction
 #'dex
 '((:implied #xCA 1 2)))

(defun dey ()
 (setf (cpu-y-register *default-cpu*) (logand #xFF (- (cpu-y-register *default-cpu*) 1)))
  (update-zn-flags (cpu-y-register *default-cpu*)))

(make-instruction
 #'dey
 '((:implied #x88 1 2)))

(defun eor (address)
  (let ((mem (read-memory address)))
    (setf (cpu-accumulator *default-cpu*) (logxor mem (cpu-accumulator *default-cpu*)))
    (update-zn-flags (cpu-accumulator *default-cpu*))))

(make-instruction
 #'eor
 '((:immediate   #x49 2 2)
   (:zero-page   #x45 2 3)
   (:zero-page-x #x55 2 4)
   (:absolute    #x4D 3 4)
   (:absolute-x  #x5D 3 4 :page-cross 1)
   (:absolute-y  #x59 3 4 :page-cross 1)
   (:indirect-x  #x41 2 6)
   (:indirect-y  #x51 2 5 :page-cross 1)))

(defun inc (address) address)
(defun inx (address) address)
(defun iny (address) address)

(defun jmp (address) address)

;; (make-instruction
;;  #'jmp
;;  '((:absolute #x4C 3 3)
;;    (:indirect #x6C 3 5)))

(defun jsr (address) address)

;; A = memory
;; LDA loads a memory value into the accumulator.
;; Flag New value
;; Z - Zero result == 0
;; N - Negative result bit 7
(defun lda (address)
  "Load A - loads a memory value into the accumulator."
  (let ((mem (read-memory address)))
    (set-acc mem)
    ;; (set-flag :zero (if (zerop mem) 1 0))
    ;; (set-flag :negative (get-bit mem 7))
    (update-zn-flags mem)))

(make-instruction
 #'lda
 '((:immediate   #xA9 2 2)
   (:zero-page   #xA5 2 3)
   (:zero-page-x #xB5 2 4)
   (:absolute    #xAD 3 4)
   (:absolute-x  #xBD 3 4 :page-cross 1)
   (:absolute-y  #xB9 3 4 :page-cross 1)
   (:indirect-x  #xA1 2 6)
   (:indirect-y  #xB1 2 5 :page-cross 1)))

(defun ldx (address)
  "Load X - loads a memory value into the X register."
  (let ((mem (read-memory address)))
    (set-x mem)
    ;; (set-flag :zero (if (zerop mem) 1 0))
    ;; (set-flag :negative (get-bit mem 7))
    (update-zn-flags mem)))

(make-instruction
 #'ldx
 '((:immediate   #xA2 2 2)
   (:zero-page   #xA6 2 3)
   (:zero-page-y #xB6 2 4)
   (:absolute    #xAE 3 4)
   (:absolute-y  #xBE 3 4 :page-cross 1)))

(defun ldy (address)
  "Load Y - loads a memory value into the Y register."
  (let ((mem (read-memory address)))
    (set-y mem)
    ;; (set-flag :zero (if (zerop mem) 1 0))
    ;; (set-flag :negative (get-bit mem 7))
    (update-zn-flags mem)))

(make-instruction
 #'ldy
 '((:immediate   #xA0 2 2)
   (:zero-page   #xA4 2 3)
   (:zero-page-x #xB4 2 4)
   (:absolute    #xAC 3 4)
   (:absolute-x  #xBC 3 4 :page-cross 1)))

(defun lsr (address) address)

(defun nop ())

;; (defun ora (mem) mem)

(defun ora (address)
  (let ((mem (read-memory address)))
    (setf (cpu-accumulator *default-cpu*) (logior mem (cpu-accumulator *default-cpu*)))
    (update-zn-flags (cpu-accumulator *default-cpu*))))

(make-instruction
 #'ora
 '((:immediate   #x09 2 2)
   (:zero-page   #x05 2 3)
   (:zero-page-x #x15 2 4)
   (:absolute    #x0D 3 4)
   (:absolute-x  #x1D 3 4 :page-cross 1)
   (:absolute-y  #x19 3 4 :page-cross 1)
   (:indirect-x  #x01 2 6)
   (:indirect-y  #x11 2 5 :page-cross 1)))

(defun pha ()
  (write-memory (+ #x0100 (cpu-stack-pointer *default-cpu*))
                (cpu-accumulator *default-cpu*))
  (dec-sp))

(make-instruction
 #'pha
 '((:implied #x48 1 3)))

(defun php ()
  (let ((flags (cpu-status-register *default-cpu*)))
    ;; break and unused always set as 1
    (setf flags (logior #x30 flags))
    (write-memory (+ #x0100 (cpu-stack-pointer *default-cpu*))
                  flags))
  (dec-sp))

(make-instruction
 #'php
 '((:implied #x08 1 3)))

(defun pla ())
(defun plp ())

(defun rol (address) address)
(defun ror (address) address)

(defun rti ())
(defun rts ())

(defun sbc ())

(defun sec ()
  (set-flag :carry 1))

(make-instruction
 #'sec
 '((:implied #x38 1 2)))

(defun sed ()
  (set-flag :decimal 1))

(make-instruction
 #'sed
 '((:implied #xF8 1 2)))

(defun sei ()
  (set-flag :interrupt 1))

(make-instruction
 #'sei
 '((:implied #x78 1 2)))

;; memory = A
;; STA stores the accumulator value into memory.
;; TODO not sure about this actually
;; TODO yeah pretty sure how I do addressing is wrong for these
;; TODO STA $80 should just store A at $80
;; TODO probably need to separate calculating the address from the fetch
(defun sta (address)
  "Store A"
  (write-memory address (cpu-accumulator *default-cpu*)))

(make-instruction
 #'sta
 '((:zero-page   #x85 2 3)
   (:zero-page-x #x95 2 4)
   (:absolute    #x8D 3 4)
   (:absolute-x  #x9D 3 5)
   (:absolute-y  #x99 3 5)
   (:indirect-x  #x81 2 6)
   (:indirect-y  #x91 2 6)))

(defun stx (address) address)
(defun sty (address) address)

(defun tax ()
  "Transfer A to X"
  (setf (cpu-x-register *default-cpu*) (cpu-accumulator *default-cpu*))
  (update-zn-flags (cpu-x-register *default-cpu*)))

(make-instruction
 #'tax
 '((:implied #xAA 1 2)))

(defun tay ()
  "Transfer A to Y"
  (setf (cpu-y-register *default-cpu*) (cpu-accumulator *default-cpu*))
  (update-zn-flags (cpu-y-register *default-cpu*)))

(make-instruction
 #'tay
 '((:implied #xA8 1 2)))

(defun tsx ()
  (setf (cpu-x-register *default-cpu*)
        (cpu-stack-pointer *default-cpu*))
  (update-zn-flags (cpu-x-register *default-cpu*)))

(make-instruction
 #'tsx
 '((:implied #xBA 1 2)))

(defun txa ()
  "Transfer X to A"
  (setf (cpu-accumulator *default-cpu*) (cpu-x-register *default-cpu*))
  (update-zn-flags (cpu-x-register *default-cpu*)))

(make-instruction
 #'txa
 '((:implied #x8A 1 2)))

(defun txs ()
  (setf (cpu-stack-pointer *default-cpu*)
        (cpu-x-register *default-cpu*)))

(make-instruction
 #'txs
 '((:implied #x9A 1 2)))

(defun tya ()
  "Transfer Y to A"
  (setf (cpu-accumulator *default-cpu*) (cpu-y-register *default-cpu*))
  (update-zn-flags (cpu-y-register *default-cpu*)))

(make-instruction
 #'tya
 '((:implied #x98 1 2)))
