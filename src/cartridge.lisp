(uiop:define-package nesoteric.cartridge
  (:use #:cl
        #:nesoteric.bits-n-bytes)
  (:export
   :ines-header
   :ines-header-prg-rom-size
   :ines-header-chr-rom-size

   :cartridge
   :cartridge-header
   :cartridge-prg-rom
   :cartridge-chr-rom

   :read-cart))



(in-package #:nesoteric.cartridge)

(defparameter *fname* "./test-roms/instr_test-v5/rom_singles/01-basics.nes")

;; (setf *fname* "./test-roms/Super Mario Bros. (World).nes")

(setf *fname* "./test-roms/instr_test-v5/rom_singles/02-implied.nes")
;; (setf *fname* "./test-roms/instr_test-v5/rom_singles/03-immediate.nes")
;; (setf *fname* "./test-roms/instr_test-v5/official_only.nes")
;; (setf *fname* "./test-roms/nestest/nestest.nes")

;; (setf *fname* "./test-roms/Super Mario Bros. (World).nes")
;; (setf *fname* "./test-roms/Kirby's Adventure (USA) (Rev 1).nes")


;; (with-open-file (stream *fname* :direction :input :element-type '(unsigned-byte 8))
;;   (let (f)
;;     (setf f  (loop for byte = (read-byte stream nil)
;;                    while byte
;;                    collect byte))
;;     (format t "~a~%" f)))

;; (char-code #\N)
;; (char-code #\E)
;; (char-code #\S)

;; (code-char 78)
;; (code-char 69)
;; (code-char 83)
;; (code-char 26)

(defun file-to-bytes (file-name)
  (with-open-file (stream file-name :direction :input :element-type 'u8)
    (let* ((byte-len (file-length stream))
           (file-array (make-array byte-len :element-type 'u8)))
      (read-sequence file-array stream)
      file-array)))

(defun read-array (stream size)
  "Reads SIZE bytes from STREAM returning length SIZE byte array"
  (let ((array (make-array size :element-type 'u8)))
    (read-sequence array stream)
    array))

(defstruct ines-header
  (magic)
  (prg-rom-size)
  (chr-rom-size)
  (flags6)
  (flags7)
  (flags8)
  (flags9)
  (flags10)
  (unused))

(defstruct flags6
  (nametable-arrangement)
  (battery-backed-prg-ram)
  (trainer)
  (alt-nametable-layout)
  (low-mapper-number))

(defstruct flags7
  (vs-unisystem)
  (playchoice-10)
  (ines-2)
  (high-mapper-number))

(defstruct flags8
  (prg-ram-size))                       ;unused?

(defstruct flags9                       ;unused?
  (tv-system)
  (reserved))

(defstruct flags10
  (tv-system-2)
  (prg-ram-present)
  (bus-conflict))

;; nes 2.0
;; flags6 looks the same

(defstruct flags7-2
  (console-type))

(defun read-ines (stream)
  (let ((header (make-ines-header)))
    (setf (ines-header-magic header) (read-array stream 4))
    (check-magic header)
    (setf (ines-header-prg-rom-size header) (read-byte stream))
    (setf (ines-header-chr-rom-size header) (read-byte stream))
    (setf (ines-header-flags6  header) (read-byte stream))
    (setf (ines-header-flags7  header) (read-byte stream))
    (setf (ines-header-flags8  header) (read-byte stream))
    (setf (ines-header-flags9  header) (read-byte stream))
    (setf (ines-header-flags10 header) (read-byte stream))
    (setf (ines-header-unused  header) (read-array stream 5))
    header))

(defun check-magic (header)
  (let ((magic (ines-header-magic header)))
        (if (and (= (char-code #\N)   (aref magic 0))
                 (= (char-code #\E)   (aref magic 1))
                 (= (char-code #\S)   (aref magic 2))
                 (= (char-code #\Sub) (aref magic 3))) ; EOF marker in DOS
            (format t "It's Magic~%")
            (format t "No Magic Found~%"))))

(defstruct cartridge
  (header)
  (prg-rom)
  (chr-rom))

(defun read-cart (&optional (file-name *fname*))
  (format t "Loading: ~a~%" file-name)
  (with-open-file (stream file-name :direction :input :element-type 'u8)
    (let ((header (read-ines stream))
          (cart (make-cartridge)))
      (setf (cartridge-header cart) header)
      (setf (cartridge-prg-rom cart)
            (read-array stream (* 16384 (ines-header-prg-rom-size header)))

            (cartridge-chr-rom cart)
            (read-array stream (* 8192 (ines-header-chr-rom-size header))))
      cart)))

#+or
(time (and (read-cart) t))

#+or
(format t "~b" (ines-header-flags6 (read-cart)))

#+or
(format t "~a~%" (read-cart))

;; (format t "~a~%" (length (cartridge-prg-rom (read-cart))))
;; (format t "~a~%" (length (cartridge-chr-rom (read-cart))))

;; (format t "~a~%" (ines-header-prg-rom-size (cartridge-header (read-cart))))
;; (format t "~a~%" (ines-header-chr-rom-size (cartridge-header (read-cart))))

;; (format t "~a~%" (length (file-to-bytes *fname*)))

;; (let ((prg (length (cartridge-prg-rom (read-cart))))
;;       (chr (length (cartridge-chr-rom (read-cart))))
;;       (len (length (file-to-bytes *fname*))))
;;   (format t "~a ~a ~a ~a ~%" prg chr len (- len prg chr)))
