(uiop:define-package nesoteric.memory
  (:use #:cl
        #:nesoteric.bits-n-bytes
        #:nesoteric.mapper)
  (:export
   :memory
   :make-memory
   :memory-ram
   :memory-mapper
   :read-memory
   :write-memory))

(in-package #:nesoteric.memory)

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

(defstruct memory
  (ram (make-array #x800 :element-type 'u8 :initial-element 0)
   :type (simple-array u8 (#x800)))
  (mapper))

(defun print-memory (memory &optional (start 0) (count 0))
  (if (> count 0)
      (format t "~a~%" (subseq (memory-ram memory) start (+ start count)))
      (dotimes (i #x0800)
        (if (= 0 (mod i 38)) (format t "~%"))
        (format t "~a " (read-memory memory i))
        (if (= i #x07FF)
            (format t "~%")))))

(defun read-memory (memory address)
  (declare
   (type memory memory)
   (type u16 address)
   (values u8))
  (cond
    ((< address #x2000)
     (aref (memory-ram memory) (logand #x07FF address)))
    ((< address #x4000)
     ;; (error "Reading PPU registers not yet implemented.")
     (format t "Reading PPU registers not yet implemented ~X~%" address)
     #xFF
     )
    ((< address #x4020)
     ;; (error "Reading APU registers not yet implemented.")
     (format t "Reading APU registers not yet implemented ~X~%" address)
     #xFF
     )
    ;; don't check beyond #FFFF as address will throw error if not u16
    ((<= address #xFFFF)
     ;; (error "Reading cartridge not yet implemented.")
     (let ((mapper-reader (mapper-memory-reader (memory-mapper memory))))
       (funcall mapper-reader address)))))

;; TODO replace redundant code with this function
(defun read-memory-u16 (memory address)
  (declare
   (type memory memory)
   (type u16 address)
   (values u8))
  (let ((mem1 (read-memory memory address))
        (mem2 (read-memory memory (1+ address))))
    (+ (ash mem2 8) mem1)))

(defun write-memory (memory address value)
  (declare
   (type memory memory)
   (type u16 address)
   (type u8 value))
  (cond
    ((< address #x2000)
     (setf (aref (memory-ram memory)
                 (logand #x07FF address)) value))
    ((< address #x4000)
     ;; (error "Writing PPU registers not yet implemented.")
     (format t "Writing PPU registers not yet implemented ~X~%" address)
     )
    ((< address #x4020)
     ;; (error "Writing APU registers not yet implemented.")
     (format t "Writing APU registers not yet implemented ~X~%" address)
     )
    ;; don't check beyond #FFFF as address will throw error if not u16
    ((<= address #xFFFF)
     ;; (error "Writing cartridge not yet implemented.")
     ;; (format t "Writing cartridge not yet implemented ~X ~X~%" address value)
     (let ((mapper-writer (mapper-memory-writer (memory-mapper memory))))
       (funcall mapper-writer address value))
     )))
