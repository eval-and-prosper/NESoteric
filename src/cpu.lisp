(uiop:define-package nesoteric.cpu
  (:use #:cl
        #:nesoteric.bits-n-bytes
        #:nesoteric.memory)

  (:import-from :alexandria
   :define-constant)

  (:export

   :cpu
   :make-cpu
   :cpu-accumulator
   :cpu-x-register
   :cpu-y-register
   :cpu-program-counter
   :cpu-stack-pointer
   :cpu-status-register
   :cpu-cycles
   :cpu-retired-instructions
   :print-cpu

   :power-on
   :cpu-step))

(in-package #:nesoteric.cpu)

;; TODO not sure if we need to care about the unused status flag
;; TODO need to remember NES is little endian
;; TODO should probably make a flags macro that sets them based on result
;; with only the flags that were specified to the macro
;; TODO (if (logand #x80 mem) 1 0) anything that looks like this is bugged

(defstruct cpu
  (accumulator          0 :type u8)
  (x-register           0 :type u8)
  (y-register           0 :type u8)
  (program-counter      0 :type u16)
  (stack-pointer        0 :type u8)
  (status-register      0 :type u8)
  (cycles               0 :type fixnum)
  (retired-instructions 0 :type fixnum))

(defparameter *instructions* (make-array #x100
                                         ;; :element-type 'function
                                         :initial-element nil))

(defun cpu-step (cpu memory &optional (log nil))
  (declare
   (type cpu cpu)
   (type memory memory))
  (let* ((opcode (read-memory memory (cpu-program-counter cpu)))
         (inst (aref *instructions* opcode)))
    (if inst
        (progn
          (when log
            (fresh-line)
            (format t "OP ~x~%" opcode))
          (inc-pc cpu)
          (funcall inst cpu memory))
        (progn
          ;; TODO probably get rid of this hack to continue after bad opcode
          (inc-pc cpu)
          (format t "Invalid Opcode ~X at ~X Cycle ~A Instruction Count ~a~%"
                  opcode
                  (cpu-program-counter cpu)
                  (cpu-cycles cpu)
                  (cpu-retired-instructions cpu))))))

(defun print-cpu (cpu)
  (declare (type cpu cpu))
  (fresh-line)
  ;; (format t "ACC ~X~%" (cpu-accumulator cpu))
  ;; (format t "X   ~X~%" (cpu-x-register cpu))
  ;; (format t "Y   ~X~%" (cpu-y-register cpu))
  ;; (format t "PC  ~X~%" (cpu-program-counter cpu))
  ;; (format t "SP  ~X~%" (cpu-stack-pointer cpu))
  ;; (format t "ST  ~X~%" (cpu-status-register cpu))
  ;; (format t "CYC ~A~%" (cpu-cycles cpu))
  (format t "A ~2,'0X X ~2,'0X Y ~2,'0X PC ~2,'0X SP ~2,'0X ST ~2,'0X CYC ~A~%"
          (cpu-accumulator cpu)
          (cpu-x-register cpu)
          (cpu-y-register cpu)
          (cpu-program-counter cpu)
          (cpu-stack-pointer cpu)
          (cpu-status-register cpu)
          (cpu-cycles cpu)))


;; (defun get-acc (&optional (cpu *default-cpu*))
;;   (cpu-accumulator cpu))
;; (defun get-x (&optional (cpu *default-cpu*))
;;   (cpu-x-register cpu))
;; (defun get-y (&optional (cpu *default-cpu*))
;;   (cpu-y-register cpu))

(defun set-acc (cpu val)
  (declare (type cpu cpu))
  (setf (cpu-accumulator cpu) val))
(defun set-x (cpu val)
  (declare (type cpu cpu))
  (setf (cpu-x-register cpu) val))
(defun set-y (cpu val)
  (declare (type cpu cpu))
  (setf (cpu-y-register cpu) val))
(defun set-pc (cpu val)
  (declare (type cpu cpu))
  (setf (cpu-program-counter cpu) val))

(defun inc-pc (cpu &optional (amount 1))
  (declare (type cpu cpu))
  (setf (cpu-program-counter cpu) (+ amount (cpu-program-counter cpu))))

(defun get-sp (cpu)
  "Get current value of the stack pointer"
  (declare (type cpu cpu))
  (cpu-stack-pointer cpu))

(defun get-sp-addr (cpu)
  "Get actual address of the stack pointer"
  (declare (type cpu cpu))
  (+ #x100 (cpu-stack-pointer cpu)))

(defun dec-sp (cpu &optional (amount 1))
  (declare (type cpu cpu))
  (setf (cpu-stack-pointer cpu)
        (logand #xFF (- (cpu-stack-pointer cpu) amount))))

(defun inc-sp (cpu &optional (amount 1))
  (declare (type cpu cpu))
 (setf (cpu-stack-pointer cpu)
        (logand #xFF (+ (cpu-stack-pointer cpu) amount))))

(define-constant +status-flags+
    '((:carry     . 0)
      (:zero      . 1)
      (:interrupt . 2)
      (:decimal   . 3)
      (:break     . 4)
      (:unused    . 5)
      (:overflow  . 6)
      (:negative  . 7))
  :test #'equal)

(defun get-flag (cpu flag-name)
  "Get the value of the keyword CPU status flag in +status-flags+"
  (declare (type cpu cpu))
  (let ((bit-position (cdr (assoc flag-name +status-flags+))))
    (unless bit-position
      (error "Invalid flag name: ~A" flag-name))
    (get-bit (cpu-status-register cpu) bit-position)))

(defun set-flag (cpu flag-name value)
  "Set the value of the keyword CPU status flag in +status-flags+"
  (declare (type cpu cpu))
  (let ((bit-position (cdr (assoc flag-name +status-flags+))))
    (unless bit-position
      (error "Invalid flag name: ~A" flag-name))
    (setf (cpu-status-register cpu)
          (set-bit (cpu-status-register cpu) bit-position value))))

(defun print-flags (cpu)
  (declare (type cpu cpu))
  (dolist (i +status-flags+)
    (format t "~a ~a~%" (get-flag cpu (car i)) (car i))))

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
(defun power-on (cpu memory)
  (setf (cpu-accumulator cpu) 0)
  (setf (cpu-x-register cpu) 0)
  (setf (cpu-y-register cpu) 0)
  (setf (cpu-program-counter cpu)
        (let ((b1 (read-memory memory #xFFFC))
              (b2 (read-memory memory #xFFFD)))
          (logior (ash b2 8) b1)))
  (setf (cpu-stack-pointer cpu) #xFD)
  (setf (cpu-status-register cpu) 4)
  (setf (cpu-cycles cpu) 7)
  ;; (setf (cpu-ram cpu) (make-array #x800
  ;;                                 :element-type 'u8
  ;;                                 :initial-element 0))
  t)

;; TODO figure out exactly how reset works
(defun reset (cpu)
  (error (format nil "reset cpu not implemented ~a" cpu)))

;; ============================================================================
;; Memory
;; ============================================================================

;; (defun print-memory (cpu &optional (start 0) (count 0))
;;   (if (> count 0)
;;       (format t "~a~%" (subseq (cpu-ram cpu) start (+ start count)))
;;       (dotimes (i #x0800)
;;         (if (= 0 (mod i 38)) (format t "~%"))
;;         (format t "~a " (read-memory cpu i))
;;         (if (= i #x07FF)
;;             (format t "~%")))))

;; TODO return type?
;; (defun read-memory (cpu address)
;;   (declare
;;    (type cpu cpu)
;;    (type u16 address))
;;   (cond
;;     ((< address #x2000)
;;      (aref (cpu-ram cpu) (logand #x07FF address)))
;;     ((< address #x4000)
;;      (error "Reading PPU registers not yet implemented."))
;;     ((< address #x4020)
;;      (error "Reading APU registers not yet implemented."))
;;     ;; don't check beyond #FFFF as address will throw error if not u16
;;     ((<= address #xFFFF)
;;      (error "Reading cartridge not yet implemented."))))

;; TODO replace redundant code with this function
;; (defun read-memory-u16 (cpu address)
;;   (declare
;;    (type cpu cpu)
;;    (type u16 address))
;;   (let ((mem1 (read-memory cpu address))
;;         (mem2 (read-memory cpu (1+ address))))
;;     (+ (ash mem2 8) mem1)))

;; (defun write-memory (cpu address value)
;;   (declare
;;    (type cpu cpu)
;;    (type u16 address)
;;    (type u8 value))
;;   (cond
;;     ((< address #x2000)
;;      (setf (aref (cpu-ram cpu)
;;                  (logand #x07FF address)) value))
;;     ((< address #x4000)
;;      (error "Writing PPU registers not yet implemented."))
;;     ((< address #x4020)
;;      (error "Writing APU registers not yet implemented."))
;;     ;; don't check beyond #FFFF as address will throw error if not u16
;;     ((<= address #xFFFF)
;;      (error "Writing cartridge not yet implemented."))))



;; ============================================================================
;; Addressing
;; ============================================================================

;; all of these assume pc already points to the first byte
(defun implied() nil)

(defun immediate (cpu memory)
  (declare
   (type cpu cpu)
   (type memory memory)
   (ignore memory))
  (let* ((pc (cpu-program-counter cpu)))
    (inc-pc cpu)
    pc))

(defun absolute (cpu memory)
  (declare
   (type cpu cpu)
   (type memory memory))
  (let* ((pc (cpu-program-counter cpu))
         (b1 (read-memory memory pc))
         (b2 (read-memory memory (1+ pc)))
         (addr (+ (ash b2 8) b1)))
    (inc-pc cpu 2)
    addr))

(defun zero-page (cpu memory)
  (declare
   (type cpu cpu)
   (type memory memory))
  (let* ((pc (cpu-program-counter cpu))
         (addr (read-memory memory pc))
         )
    (inc-pc cpu)
    addr))

;; TODO check overflow
(defun absolute-x (cpu memory)
  (declare
   (type cpu cpu)
   (type memory memory))
  (let* ((pc (cpu-program-counter cpu))
         (b1 (read-memory memory pc))
         (b2 (read-memory memory (1+ pc)))
         (addr (+ (ash b2 8) b1 (cpu-x-register cpu)))
         )
    (inc-pc cpu 2)
    addr))

;; TODO check overflow
(defun absolute-y (cpu memory)
  (declare
   (type cpu cpu)
   (type memory memory))
  (let* ((pc (cpu-program-counter cpu))
         (b1 (read-memory memory pc))
         (b2 (read-memory memory (1+ pc)))
         ;; TODO buggy as I forgot to wrap this with the logand causing bad number
         ;; need to track down all the code like this and replace it
         (addr (logand #xFF (+ (ash b2 8) b1 (cpu-y-register cpu))))
         )
    (inc-pc cpu 2)
    addr))

;; zero-page,x
(defun zero-page-x (cpu memory)
  (declare
   (type cpu cpu)
   (type memory memory))
  (let* ((pc (cpu-program-counter cpu))
         (zp (read-memory memory pc))
         ;; (mem (read-memory (logand #xFF (+ addr (cpu-x-register cpu)))))
         (addr (logand #xFF (+ zp (cpu-x-register cpu)))))
    (inc-pc cpu)
    ;; mem
    addr))

;; zero-page,y
(defun zero-page-y (cpu memory)
  (declare
   (type cpu cpu)
   (type memory memory))
  (let* ((pc (cpu-program-counter cpu))
         (zp (read-memory memory pc))
         ;; (mem (read-memory (logand #xFF (+ addr (cpu-y-register cpu)))))
         (addr (logand #xFF (+ zp (cpu-y-register cpu)))))
    (inc-pc cpu)
    ;; mem
    addr))

(defun indirect (cpu memory)
  (declare
   (type cpu cpu)
   (type memory memory))
  (let* ((pc (cpu-program-counter cpu))
         (b1 (read-memory memory pc))
         (b2 (read-memory memory (1+ pc)))
         (addr (+ (ash b2 8) b1))
         (mem1 (read-memory memory addr))
         (mem2 (read-memory memory (1+ addr))))
    (inc-pc cpu 2)
    (+ (ash mem2 8) mem1)))

(defun indirect-bug (cpu memory)
  (declare
   (type cpu cpu)
   (type memory memory))
  "When least significant address byte ends in #xFF it reads that value from XX00 instead of wrapping the high byte"
  (let* ((pc (cpu-program-counter cpu))
         (b1 (read-memory memory pc))
         (b2 (read-memory memory (1+ pc)))
         (addr     (+ (ash b2 8) b1))
         (bug-addr (+ (ash b2 8) #x00))
         (bugged (if (= #xFF b1) t nil)))
    (let ((mem1 (read-memory memory addr))
          (mem2 (read-memory memory (if bugged
                                 bug-addr
                                 (1+ addr)))))
      (inc-pc cpu 2)
      (+ (ash mem2 8) mem1))))

;; (zero-page,x)
;; TODO no overflow?
(defun pre-indexed-indirect (cpu memory)
  (declare
   (type cpu cpu)
   (type memory memory))
  (let* ((pc (cpu-program-counter cpu))
         (op (logand #xFF (+ (read-memory memory pc) (cpu-x-register cpu))))
         (b1 (read-memory memory op))
         (b2 (read-memory memory (logand #xFF (1+ op))))
         (addr (+ (ash b2 8) b1)))
    (inc-pc cpu)
    addr))

;; (zero-page),y
;; TODO check overflow
(defun post-indexed-indirect (cpu memory)
  (declare
   (type cpu cpu)
   (type memory memory))
  (let* ((pc (cpu-program-counter cpu))
         (op (read-memory memory pc))
         (b1 (read-memory memory op))
         (b2 (read-memory memory (1+ op)))
         (addr (logand #xFF (+ (ash b2 8) b1 (cpu-y-register cpu)))))
    (inc-pc cpu)
    addr))

(defun relative (cpu memory)
  (declare
   (type cpu cpu)
   (type memory memory))
  (let* ((pc (cpu-program-counter cpu))
         (offset (u8-to-s8 (read-memory memory pc))))
    (inc-pc cpu)
    offset))

;; Should be a constant but it looks like that's impossible
(defparameter +addressing-modes+
  `((:implied      nil)
    (:accumulator  nil)
    (:relative     ,#'relative)
    (:immediate    ,#'immediate)
    (:absolute     ,#'absolute)
    (:zero-page    ,#'zero-page)
    (:absolute-x   ,#'absolute-x)
    (:absolute-y   ,#'absolute-y)
    (:zero-page-x  ,#'zero-page-x)
    (:zero-page-y  ,#'zero-page-y)
    (:indirect     ,#'indirect)
    (:indirect-bug ,#'indirect-bug)
    (:indirect-x   ,#'pre-indexed-indirect)
    (:indirect-y   ,#'post-indexed-indirect)
    ;; (:pre-indexed-indirect ,#'pre-indexed-indirect)
    ;; (:post-indexed-indirect ,#'post-indexed-indirect)
    ))

;; =============================================================================
;; Instructions
;; =============================================================================

(defun update-z-flag (cpu value)
  (set-flag cpu :zero (if (zerop value) 1 0)))

(defun update-n-flag (cpu value)
  (set-flag cpu :negative (get-bit value 7)))

(defun update-zn-flags (cpu value)
  (set-flag cpu :zero (if (zerop value) 1 0))
  (set-flag cpu :negative (get-bit value 7)))

(defun update-cycles (cpu cycles)
  (declare
   (type fixnum cycles))
  (incf (cpu-cycles cpu) cycles)
  (incf (cpu-retired-instructions cpu)))

(defun make-instruction (inst-fun inst-list)
  (dolist (inst inst-list)
    (let* ((addr-mode (second (assoc (first inst) +addressing-modes+)))
           (opcode (second inst))
           (bytes (third inst))
           (cycles (fourth inst)))
      (when (aref *instructions* opcode)
        (format t "Redefining instruction: ~X" opcode))
      (setf (aref *instructions* opcode)
            (cond
              ((or (eql :accumulator (first inst)))
               (lambda (cpu memory)
                 (declare (ignore memory))
                 (funcall inst-fun cpu)
                 (update-cycles cpu cycles)))

              ((eql :implied (first inst))
               (lambda (cpu memory)
                 (funcall inst-fun cpu memory)
                 (update-cycles cpu cycles)))

              ((eql :relative (first inst))
               (lambda (cpu memory)
                 (let ((offset (funcall addr-mode cpu memory)))
                   (funcall inst-fun cpu offset)
                   (update-cycles cpu cycles))))

              (t
               (lambda (cpu memory)
                 (when (null addr-mode) (error (format nil "ADDR ~a" addr-mode)))
                 (let ((address (funcall addr-mode cpu memory)))
                   (funcall inst-fun cpu memory address)
                   (update-cycles cpu cycles)))))
            ;; (if (or (eql :accumulator (first inst))
            ;;         (eql :implied (first inst))
            ;;         (eql :rel))
            ;;     (lambda (cpu memory)
            ;;       (funcall inst-fun cpu))
            ;;     (lambda (cpu memory)
            ;;       (let ((address (funcall addr-mode cpu memory)))
            ;;         (funcall inst-fun cpu memory address))))
            ))))

;; adc add with carry
;; A = A + memory + C
(defun adc (cpu memory address)
  ;; (declare (type u8 mem))
  (let* ((A (cpu-accumulator cpu))
         (carry (get-flag cpu :carry))
         (mem (read-memory memory address))
         (res (+ A mem carry)))
    (declare (type u8 A)
             (type bit carry))

    (set-flag cpu :carry (if (> res #xFF) 1 0))
    (setf res (logand res #xFF))

    (set-flag cpu :zero (if (zerop res) 1 0))

    ;; (result ^ A) & (result ^ memory) & $80
    ;; Formula detects overflow by checking sign changes:
    ;; XOR sets bit 7 to 1 when operands have different signs
    ;; ANDing the XORs will be 1 in bit 7 only if result differs from both inputs
    ;; Final & $80 isolates bit 7 (the sign bit)
    ;; We then shift to extract the bit and set the flag with it
    (set-flag cpu :overflow
              (ash (logand (logxor res A)
                           (logxor res mem)
                           #x80)
                   -7))
    (set-flag cpu :negative (get-bit res 7))
    (set-acc cpu res)))

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

(defun inst-and (cpu memory address)
  (let* ((mem (read-memory memory address))
        (result (logand mem (cpu-accumulator cpu))))
    (update-zn-flags cpu result)
    (set-acc cpu result)))

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

(defun asl (cpu memory address)
  (let* ((mem (read-memory memory address))
         ;; (carry (if (logand #x80 mem) 1 0))
         (carry (get-bit mem 7))
         (res (logand #xFF (ash mem 1))))
    (set-flag cpu :carry carry)
    (update-zn-flags cpu res)
    (write-memory memory address res)))

(make-instruction
 #'asl
 '((:zero-page   #x06 2 5)
   (:zero-page-x #x16 2 6)
   (:absolute    #x0E 3 6)
   (:absolute-x  #x1E 3 7)))

(defun asl-a (cpu)
  (let* ((mem (cpu-accumulator cpu))
         ;; (carry (if (logand #x80 mem) 1 0))
         (carry (get-bit mem 7))
         (res (logand #xFF (ash mem 1))))
    (set-flag cpu :carry carry)
    (update-zn-flags cpu res)
    (setf (cpu-accumulator cpu) res)))

(make-instruction
 #'asl-a
 '((:accumulator #x0A 1 2)))


(defun bcc (cpu offset)
  (let ((carry (get-flag cpu :carry)))
    (when (= carry 0)
      ;; TODO extra cycle if branch taken
      (setf (cpu-program-counter cpu)
            (logand #xFFFF (+ (cpu-program-counter cpu) offset))))))

(make-instruction
 #'bcc
 '((:relative #x90 2 2 :page-cross 1)))

(defun bcs (cpu offset)
  (let ((carry (get-flag cpu :carry)))
    (when (> carry 0)
      ;; TODO extra cycle if branch taken
      (setf (cpu-program-counter cpu)
            (logand #xFFFF (+ (cpu-program-counter cpu) offset))))))

(make-instruction
 #'bcs
 '((:relative #xB0 2 2 :page-cross 1)))

(defun beq (cpu offset)
  (let ((zero (get-flag cpu :zero)))
    (when (> zero 0)
      ;; TODO extra cycle if branch taken
      (setf (cpu-program-counter cpu)
            (logand #xFFFF (+ (cpu-program-counter cpu) offset))))))

(make-instruction
 #'beq
 '((:relative #xF0 2 2 :page-cross 1)))


(defun inst-bit (cpu memory address)
  (let* ((mem (read-memory memory address))
         (res (logand mem (cpu-accumulator cpu))))
    (set-flag cpu :overflow (if (> (logand #x40 mem) 0) 1 0))
    (update-z-flag cpu res)
    (update-n-flag cpu mem)))

(make-instruction
 #'inst-bit
 '((:zero-page #x24 2 3)
   (:absolute  #x2C 3 4)))

(defun bmi (cpu offset)
  (let ((neg (get-flag cpu :negative)))
    (when (> neg 0)
      ;; TODO extra cycle if branch taken
      (setf (cpu-program-counter cpu)
            (logand #xFFFF (+ (cpu-program-counter cpu) offset))))))

(make-instruction
 #'bmi
 '((:relative #x30 2 2 :page-cross 1)))

(defun bne (cpu offset)
  (let ((zero (get-flag cpu :zero)))
    (when (= zero 0)
      ;; TODO extra cycle if branch taken
      (setf (cpu-program-counter cpu)
            (logand #xFFFF (+ (cpu-program-counter cpu) offset))))))

(make-instruction
 #'bne
 '((:relative #xD0 2 2 :page-cross 1)))

(defun bpl (cpu offset)
  (let ((neg (get-flag cpu :negative)))
    (when (= neg 0)
      ;; TODO extra cycle if branch taken
      (setf (cpu-program-counter cpu)
            (logand #xFFFF (+ (cpu-program-counter cpu) offset))))))

(make-instruction
 #'bpl
 '((:relative #x10 2 2 :page-cross 1)))

;; TODO implement
;; TODO Trigger an interrupt
;; TODO implement buggy behavior when NMI occurs at the same time
;; TODO need to implement cart to be able to read FFFE and FFFF
(defun brk (cpu memory)
  ;; push PC
  (let ((pc (logand #xFFFF (- (cpu-program-counter cpu) 1))))
    (write-memory memory (get-sp-addr cpu) (ash (logand #xFF00 pc) -8))
    (dec-sp cpu)
    (write-memory memory (get-sp-addr cpu) (logand #x00FF pc))
    (dec-sp cpu))

  ;; push flags
  (let ((flags (cpu-status-register cpu)))
    ;; break and unused always set as 1
    (setf flags (logior #x30 flags))
    (write-memory memory (get-sp-addr cpu) flags)
    (dec-sp cpu))

  ;; set interrupt flag only after pushing current flags
  (set-flag cpu :interrupt 1)

  ;; fetch IRQ handler address
  (let ((low  (read-memory memory #xFFFE))
        (high (read-memory memory #xFFFF)))
    ;; set IRQ handler to PC
    (setf (cpu-program-counter cpu)
          (+ (ash high 8) low))))

(make-instruction
 #'brk
 '((:implied #x00 1 7)))

(defun bvc (cpu offset)
  (let ((ov (get-flag cpu :overflow)))
    (when (= ov 0)
      ;; TODO extra cycle if branch taken
      (setf (cpu-program-counter cpu)
            (logand #xFFFF (+ (cpu-program-counter cpu) offset))))))

(make-instruction
 #'bvc
 '((:relative #x50 2 2 :page-cross 1)))

(defun bvs (cpu offset)
  (let ((ov (get-flag cpu :overflow)))
    (when (> ov 0)
      ;; TODO extra cycle if branch taken
      (setf (cpu-program-counter cpu)
            (logand #xFFFF (+ (cpu-program-counter cpu) offset))))))

(make-instruction
 #'bvs
 '((:relative #x70 2 2 :page-cross 1)))

(defun clc (cpu memory)
  (declare (ignore memory))
  (set-flag cpu :carry 0))

(make-instruction
 #'clc
 '((:implied #x18 1 2)))

(defun cld (cpu memory)
  (declare (ignore memory))
  (set-flag cpu :decimal 0))

(make-instruction
 #'cld
 '((:implied #xD8 1 2)))

(defun cli (cpu memory)
  (declare (ignore memory))
  (set-flag cpu :interrupt 0))

(make-instruction
 #'cli
 '((:implied #x58 1 2)))

(defun clv (cpu memory)
  (declare (ignore memory))
  (set-flag cpu :overflow 0))

(make-instruction
 #'clv
 '((:implied #xB8 1 2)))


(defun cmp-base (cpu mem reg)
  (set-flag cpu :carry (if (>= reg mem) 1 0))
  (set-flag cpu :zero (if (= reg mem) 1 0))
  (set-flag cpu :negative (if (> (logand #x80 (- reg mem)) 0) 1 0)))

(defun cmp (cpu memory address)
  (let ((mem (read-memory memory address))
        (a (cpu-accumulator cpu)))
    (cmp-base cpu mem a)))

(make-instruction
 #'cmp
 '((:immediate   #xC9 2 2)
   (:zero-page   #xC5 2 3)
   (:zero-page-x #xD5 2 4)
   (:absolute    #xCD 3 4)
   (:absolute-x  #xDD 3 4 :page-cross 1)
   (:absolute-y  #xD9 3 4 :page-cross 1)
   (:indirect-x  #xC1 2 6)
   (:indirect-y  #xD1 2 5 :page-cross 1)))

(defun cpx (cpu memory address)
  (let ((mem (read-memory memory address))
        (x (cpu-x-register cpu)))
    (cmp-base cpu mem x)))

(make-instruction
 #'cpx
 '((:immediate #xE0 2 2)
   (:zero-page #xE4 2 3)
   (:absolute  #xEC 3 4)))

(defun cpy (cpu memory address)
  (let ((mem (read-memory memory address))
        (y (cpu-y-register cpu)))
    (cmp-base cpu mem y)))

(make-instruction
 #'cpy
 '((:immediate #xC0 2 2)
   (:zero-page #xC4 2 3)
   (:absolute  #xCC 3 4)))

(defun dec (cpu memory address)
  (let* ((mem (read-memory memory address))
         (res (logand #xFF (1- mem))))
    (update-zn-flags cpu res)
    (write-memory memory address res)))

(make-instruction
 #'dec
 '((:zero-page   #xC6 2 5)
   (:zero-page-x #xD6 2 6)
   (:absolute    #xCE 3 6)
   (:absolute-x  #xDE 3 7)))

(defun dex (cpu memory)
  (declare (ignore memory))
  (setf (cpu-x-register cpu) (logand #xFF (1- (cpu-x-register cpu))))
  (update-zn-flags cpu (cpu-x-register cpu)))

(make-instruction
 #'dex
 '((:implied #xCA 1 2)))

(defun dey (cpu memory)
  (declare (ignore memory))
 (setf (cpu-y-register cpu) (logand #xFF (1- (cpu-y-register cpu))))
  (update-zn-flags cpu (cpu-y-register cpu)))

(make-instruction
 #'dey
 '((:implied #x88 1 2)))

(defun eor (cpu memory address)
  (let ((mem (read-memory memory address)))
    (setf (cpu-accumulator cpu) (logxor mem (cpu-accumulator cpu)))
    (update-zn-flags cpu (cpu-accumulator cpu))))

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

(defun inc (cpu memory address)
  (let* ((mem (read-memory memory address))
         (res (logand #xFF (1+ mem))))
    (update-zn-flags cpu res)
    (write-memory memory address res)))

(make-instruction
 #'inc
 '((:zero-page   #xE6 2 5)
   (:zero-page-x #xF6 2 6)
   (:absolute    #xEE 3 6)
   (:absolute-x  #xFE 3 7)))

(defun inx (cpu memory)
  (declare (ignore memory))
  (setf (cpu-x-register cpu) (logand #xFF (1+ (cpu-x-register cpu))))
  (update-zn-flags cpu (cpu-x-register cpu)))

(make-instruction
 #'inx
 '((:implied #xE8 1 2)))

(defun iny (cpu memory)
  (declare (ignore memory))
  (setf (cpu-y-register cpu) (logand #xFF (1+ (cpu-y-register cpu))))
  (update-zn-flags cpu (cpu-y-register cpu)))

(make-instruction
 #'iny
 '((:implied #xC8 1 2)))

(defun jmp (cpu memory address)
  (declare (ignore memory))
  (setf (cpu-program-counter cpu) address))

(make-instruction
 #'jmp
 '((:absolute     #x4C 3 3)
   (:indirect-bug #x6C 3 5)))

(defun jsr (cpu memory address)
  (let ((pc (logand #xFFFF (- (cpu-program-counter cpu) 1))))
    (write-memory memory (get-sp-addr cpu) (ash (logand #xFF00 pc) -8))
    (dec-sp cpu)
    (write-memory memory (get-sp-addr cpu) (logand #x00FF pc))
    (dec-sp cpu)
    (setf (cpu-program-counter cpu) address)))

(make-instruction
 #'jsr
 '((:absolute #x20 3 6)))

;; A = memory
;; LDA loads a memory value into the accumulator.
;; Flag New value
;; Z - Zero result == 0
;; N - Negative result bit 7
(defun lda (cpu memory address)
  "Load A - loads a memory value into the accumulator."
  (let ((mem (read-memory memory address)))
    (set-acc cpu mem)
    (update-zn-flags cpu mem)))

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

(defun ldx (cpu memory address)
  "Load X - loads a memory value into the X register."
  (let ((mem (read-memory memory address)))
    (set-x cpu mem)
    (update-zn-flags cpu mem)))

(make-instruction
 #'ldx
 '((:immediate   #xA2 2 2)
   (:zero-page   #xA6 2 3)
   (:zero-page-y #xB6 2 4)
   (:absolute    #xAE 3 4)
   (:absolute-y  #xBE 3 4 :page-cross 1)))

(defun ldy (cpu memory address)
  "Load Y - loads a memory value into the Y register."
  (let ((mem (read-memory memory address)))
    (set-y cpu mem)
    (update-zn-flags cpu mem)))

(make-instruction
 #'ldy
 '((:immediate   #xA0 2 2)
   (:zero-page   #xA4 2 3)
   (:zero-page-x #xB4 2 4)
   (:absolute    #xAC 3 4)
   (:absolute-x  #xBC 3 4 :page-cross 1)))

(defun lsr (cpu memory address)
  (let* ((mem (read-memory memory address))
         ;; (carry (if (logand #x1 mem) 1 0))
         (carry (get-bit mem 0))
         (res (logand #xFF (ash mem -1))))
    (set-flag cpu :carry carry)
    (update-zn-flags cpu res)
    (write-memory memory address res)))

(make-instruction
 #'lsr
 '((:zero-page   #x46 2 5)
   (:zero-page-x #x56 2 6)
   (:absolute    #x4E 3 6)
   (:absolute-x  #x5E 3 7)))

(defun lsr-a (cpu)
  (let* ((mem (cpu-accumulator cpu))
         ;; (carry (if (logand #x01 mem) 1 0))
         (carry (get-bit mem 0))
         (res (logand #xFF (ash mem -1))))
    (set-flag cpu :carry carry)
    (update-zn-flags cpu res)
    (setf (cpu-accumulator cpu) res)))

(make-instruction
 #'lsr-a
 '((:accumulator #x4A 1 2)))


(defun nop (cpu memory)
  (declare (ignore cpu) (ignore memory)))

(make-instruction
 #'nop
 '((:implied #xEA 1 2)))

;; (defun ora (mem) mem)

(defun ora (cpu memory address)
  (let ((mem (read-memory memory address)))
    (setf (cpu-accumulator cpu) (logior mem (cpu-accumulator cpu)))
    (update-zn-flags cpu (cpu-accumulator cpu))))

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

(defun pha (cpu memory)
  (write-memory memory
                (+ #x0100 (cpu-stack-pointer cpu))
                (cpu-accumulator cpu))
  (dec-sp cpu))

(make-instruction
 #'pha
 '((:implied #x48 1 3)))

(defun php (cpu memory)
  (let ((flags (cpu-status-register cpu)))
    ;; break and unused always set as 1
    (setf flags (logior #x30 flags))
    (write-memory memory
                  (+ #x0100 (cpu-stack-pointer cpu))
                  flags))
  (dec-sp cpu))

(make-instruction
 #'php
 '((:implied #x08 1 3)))

(defun pla (cpu memory)
  (inc-sp cpu)
  (let ((mem (read-memory memory (get-sp-addr cpu))))
    (setf (cpu-accumulator cpu) mem)
    (update-zn-flags cpu mem)))

(make-instruction
 #'pla
 '((:implied #x68 1 4)))

(defun plp (cpu memory)
  (inc-sp cpu)
  (let ((mem (read-memory memory (get-sp-addr cpu)))
        (b (get-flag cpu :break))
        (u (get-flag cpu :unused)))
    (setf (cpu-status-register cpu) mem)
    (set-flag cpu :break b)
    (set-flag cpu :unused u)))

(make-instruction
 #'plp
 '((:implied #x28 1 4)))

(defun rol (cpu memory address)
  (let* ((mem (read-memory memory address))
         ;; (carry-out (if (logand #x80 mem) 1 0))
         (carry-out (get-bit mem 7))
         (carry-in (get-flag cpu :carry))
         (res (+ carry-in (logand #xFF (ash mem 1)))))
    (set-flag cpu :carry carry-out)
    (update-zn-flags cpu res)
    (write-memory memory address res)))

(make-instruction
 #'rol
 '((:zero-page   #x26 2 5)
   (:zero-page-x #x36 2 6)
   (:absolute    #x2E 3 6)
   (:absolute-x  #x3E 3 7)))

(defun rol-a (cpu)
  (let* ((mem (cpu-accumulator cpu))
         ;; (carry-out (if (logand #x80 mem) 1 0))
         (carry-out (get-bit mem 7))
         (carry-in (get-flag cpu :carry))
         (res (+ carry-in (logand #xFF (ash mem 1)))))
    (set-flag cpu :carry carry-out)
    (update-zn-flags cpu res)
    (setf (cpu-accumulator cpu) res)))

(make-instruction
 #'rol-a
 '((:accumulator #x2A 1 2)))

(defun ror (cpu memory address)
  (let* ((mem (read-memory memory address))
         (carry-out (get-bit mem 0))
         (carry-in (get-flag cpu :carry))
         (res (logand #xFF (ash mem -1))) )
    (setf res (set-bit res 7 carry-in))
    (set-flag cpu :carry carry-out)
    (update-zn-flags cpu res)
    (write-memory memory address res)))

(make-instruction
 #'ror
 '((:zero-page   #x66 2 5)
   (:zero-page-x #x76 2 6)
   (:absolute    #x6E 3 6)
   (:absolute-x  #x7E 3 7)))

(defun ror-a (cpu)
  (let* ((mem (cpu-accumulator cpu))
         (carry-out (get-bit mem 0))
         (carry-in (get-flag cpu :carry))
         (res (logand #xFF (ash mem -1))))
    (setf res (set-bit res 7 carry-in))
    (set-flag cpu :carry carry-out)
    (update-zn-flags cpu res)
    (setf (cpu-accumulator cpu) res)))

(make-instruction
 #'ror-a
 '((:accumulator #x6A 1 2)))

;; TODO interrupt disable flag applies immediately
(defun rti (cpu memory)
  (plp cpu memory)
  (rts cpu memory)
  (inc-pc cpu -1))

(make-instruction
 #'rti
 '((:implied #x40 1 6)))

(defun rts (cpu memory)
  (let (pc-low pc-high pc)
    (inc-sp cpu)
    (setf pc-low  (read-memory memory (get-sp-addr cpu)))
    (inc-sp cpu)
    (setf pc-high (read-memory memory (get-sp-addr cpu)))
    ;; TODO Should we add 1 here? I think it's supposed to be off by one?
    (setf pc (+ (ash pc-high 8) pc-low 1))
    (setf (cpu-program-counter cpu) pc)))

(make-instruction
 #'rts
 '((:implied #x60 1 6)))

(defun sbc (cpu memory address)
  (let* ((mem (read-memory memory address))
         (carry (get-flag cpu :carry))
         (a (cpu-accumulator cpu))
         (mem-not (logand #xFF (lognot mem)))
         (res (+ a mem-not carry)))

    (set-flag cpu :carry (if (> mem a) 0 1))
    (setf res (logand #xFF res))

    ;; (result ^ A) & (result ^ ~memory) & $80
    (set-flag cpu :overflow
              (ash (logand (logxor res A)
                           (logxor res mem-not)
                           #x80)
                   -7))
    (update-zn-flags cpu res)
    (setf (cpu-accumulator cpu) res)))

(make-instruction
 #'sbc
 '((:immediate   #xE9 2 2)
   (:zero-page   #xE5 2 3)
   (:zero-page-x #xF5 2 4)
   (:absolute    #xED 3 4)
   (:absolute-x  #xFD 3 4 :page-cross 1)
   (:absolute-y  #xF9 3 4 :page-cross 1)
   (:indirect-x  #xE1 2 6)
   (:indirect-y  #xF1 2 5 :page-cross 1)))


(defun sec (cpu memory)
  (declare (ignore memory))
  (set-flag cpu :carry 1))

(make-instruction
 #'sec
 '((:implied #x38 1 2)))

(defun sed (cpu memory)
  (declare (ignore memory))
  (set-flag cpu :decimal 1))

(make-instruction
 #'sed
 '((:implied #xF8 1 2)))

(defun sei (cpu memory)
  (declare (ignore memory))
  (set-flag cpu :interrupt 1))

(make-instruction
 #'sei
 '((:implied #x78 1 2)))

;; memory = A
;; STA stores the accumulator value into memory.
(defun sta (cpu memory address)
  "Store A"
  (write-memory memory address (cpu-accumulator cpu)))

(make-instruction
 #'sta
 '((:zero-page   #x85 2 3)
   (:zero-page-x #x95 2 4)
   (:absolute    #x8D 3 4)
   (:absolute-x  #x9D 3 5)
   (:absolute-y  #x99 3 5)
   (:indirect-x  #x81 2 6)
   (:indirect-y  #x91 2 6)))

(defun stx (cpu memory address)
  (write-memory memory address (cpu-x-register cpu)))

(make-instruction
 #'stx
 '((:zero-page   #x86 2 3)
   (:zero-page-x #x96 2 4)
   (:absolute    #x8E 3 4)))

(defun sty (cpu memory address)
  (write-memory memory address (cpu-y-register cpu)))

(make-instruction
 #'sty
 '((:zero-page   #x84 2 3)
   (:zero-page-x #x94 2 4)
   (:absolute    #x8C 3 4)))

(defun tax (cpu memory)
  "Transfer A to X"
  (declare (ignore memory))
  (setf (cpu-x-register cpu) (cpu-accumulator cpu))
  (update-zn-flags cpu (cpu-x-register cpu)))

(make-instruction
 #'tax
 '((:implied #xAA 1 2)))

(defun tay (cpu memory)
  "Transfer A to Y"
  (declare (ignore memory))
  (setf (cpu-y-register cpu) (cpu-accumulator cpu))
  (update-zn-flags cpu (cpu-y-register cpu)))

(make-instruction
 #'tay
 '((:implied #xA8 1 2)))

(defun tsx (cpu memory)
  (declare (ignore memory))
  (setf (cpu-x-register cpu)
        (cpu-stack-pointer cpu))
  (update-zn-flags cpu (cpu-x-register cpu)))

(make-instruction
 #'tsx
 '((:implied #xBA 1 2)))

(defun txa (cpu memory)
  "Transfer X to A"
  (declare (ignore memory))
  (setf (cpu-accumulator cpu) (cpu-x-register cpu))
  (update-zn-flags cpu (cpu-x-register cpu)))

(make-instruction
 #'txa
 '((:implied #x8A 1 2)))

(defun txs (cpu memory)
  (declare (ignore memory))
  (setf (cpu-stack-pointer cpu)
        (cpu-x-register cpu)))

(make-instruction
 #'txs
 '((:implied #x9A 1 2)))

(defun tya (cpu memory)
  "Transfer Y to A"
  (declare (ignore memory))
  (setf (cpu-accumulator cpu) (cpu-y-register cpu))
  (update-zn-flags cpu (cpu-y-register cpu)))

(make-instruction
 #'tya
 '((:implied #x98 1 2)))
