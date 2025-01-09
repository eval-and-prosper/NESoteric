(uiop:define-package nesoteric.mapper
  (:use #:cl
        #:nesoteric.bits-n-bytes
        #:nesoteric.cartridge)
  (:export
   :make-mapper
   :mapper-factory
   :mapper-prg-rom
   :mapper-chr-rom
   :mapper-cart-ram
   :mapper-memory-reader
   :mapper-memory-writer))

(in-package #:nesoteric.mapper)

;; (defmethod read-mapper ())

;; (defmethod write-mapper ())

(defstruct mapper
  (prg-rom)
  (chr-rom)
  (cart-ram)
  (memory-reader)
  (memory-writer))

;; ============================================================================
;; Mapper 0
;; ============================================================================
;; All Banks are fixed,
;; CPU $6000-$7FFF: Family Basic only: PRG RAM, mirrored as necessary to fill entire 8 KiB window, write protectable with an external switch
;; CPU $8000-$BFFF: First 16 KB of ROM.
;; CPU $C000-$FFFF: Last 16 KB of ROM (NROM-256) or mirror of $8000-$BFFF (NROM-128).
;; Horizontal mirroring : 'H' disconnected, 'V' connected.
;; Vertical mirroring : 'H' connected, 'V' disconnected.

(defun mapper-factory (cart)
  (mapper-0 cart))

(defun mapper-0 (cart)
  (let* ((mapper (make-mapper))
         (header (cartridge-header cart))
         (prg-banks (ines-header-prg-rom-size header))
         (chr-banks (ines-header-chr-rom-size header)))

    ;; provide full 8kb ram at all times
    (setf (mapper-cart-ram mapper)
          (make-array #x2000 :element-type 'u8 :initial-element 0))

    (setf (mapper-prg-rom mapper) (cartridge-prg-rom cart))
    (setf (mapper-chr-rom mapper) (cartridge-chr-rom cart))
    (setf (mapper-memory-reader mapper)
          (lambda (addr) (mapper-0-reader mapper prg-banks addr)))
    (setf (mapper-memory-writer mapper)
          (lambda (addr value) (mapper-0-writer mapper addr value)))
    mapper))

(defun mapper-0-reader (mapper prg-banks address)
  (cond
     ;; RAM
    ((and (<= #x6000 address)
          (<= address #x7FFF))
     (aref (mapper-cart-ram mapper) (- address #x6000)))

    ((and (<= #x8000 address)
          (<= address #xFFFF))
     (let ((addr-mirror (if (= prg-banks 1)
                            (logand address #x3FFF)
                            (- address #x8000))))
       (aref (mapper-prg-rom mapper) addr-mirror)))

    (t (format t "Bad Read Address: ~X~%" address)))

  ;; (let ((proper-address (- address #x8000)))
  ;;   (if (< proper-address 0)
  ;;       (format t "Bad Read Address: ~a~%Bad Trans: ~a~%" address proper-address)
  ;;       (aref (mapper-prg-rom mapper) proper-address)))
  )

(defun mapper-0-writer (mapper address value)
  (if (and (<= #x6000 address)
           (<= address #x7FFF))
      (setf (aref (mapper-cart-ram mapper) (- address #x6000))
            value)
      (format t "Mapper Bad Write Address: ~X Val: ~X~%" address value)))





;; (defvar *m* (mapper-factory (read-cart)))

;; (format t "~a~%" *m*)

;; (funcall (mapper-memory-reader *m*) 5)
;; (funcall (mapper-memory-writer *m*) 9)
