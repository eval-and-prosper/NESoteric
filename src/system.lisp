(uiop:define-package nesoteric.system
  (:use #:cl
        #:nesoteric.cpu
        #:nesoteric.bits-n-bytes
        #:nesoteric.cartridge
        #:nesoteric.memory
        #:nesoteric.mapper))

(in-package #:nesoteric.system)

;; probably have a struct that holds everything cpu ppu apu mem cart etc
;; move the memory functions here so they can reference everything

(defstruct system
  (cpu)
  (cartridge)
  (memory))


(defun init-system ()
  (let ((sys (make-system))
        (mem (make-memory))
        (cart (read-cart)))
    (setf (system-cpu sys) (make-cpu))
    (setf (system-memory sys) mem)
    (setf (system-cartridge sys) cart)
    (setf (memory-mapper mem) (mapper-factory cart))
    sys))

(defun run-system ()
  (let* ((sys (init-system))
         (mem (system-memory sys))
         (cpu (system-cpu sys))
         (inst 0)
         (log t))
    ;; (read-memory mem #xFFFD)
    ;; (format t "~a~%" (read-memory mem #x6B27))
    ;; (format t "~x~%" (read-memory mem #xE683))
    ;; (format t "~a~%" (read-memory mem #xE683))

    (power-on cpu mem)
    ;; (setf (cpu-program-counter cpu) #xC000)
    ;; (print-cpu cpu)
    (dotimes (i 50000)
      ;; (when (/= #x80 (read-memory mem #x6000))
      ;;   (return))
      (when ;; (= #xE8D5 (cpu-program-counter cpu))
          ;; (= #xE976 (cpu-program-counter cpu))
          ;; (= #xE976 (cpu-program-counter cpu))
          (= #xEBA2 (cpu-program-counter cpu))
        (format t "DONE~%")
        (return))
      (when log
        (format t "========== PC ~X~%" (cpu-program-counter cpu)))
      (cpu-step cpu mem log)
      (when log
        (print-cpu cpu))
      (incf inst)
      ;; (handler-case
      ;;     (cpu-step cpu mem)
      ;;   (error (e)
      ;;     (format t "An error occurred: ~a~%" e)
      ;;     (print-cpu cpu)
      ;;     (return)))
      )
    (format t "Executed ~a Instructions" inst)
    (print-cpu cpu)
    (fresh-line)
    ;; (format t "x02 ~a~%" (read-memory mem #x02))
    ;; (format t "x03 ~a~%" (read-memory mem #x03))

    (format t "status ~x~%" (read-memory mem #x6000))
    (dotimes (i 10)
      (format t "~a" (code-char (read-memory mem (+ #x6003 i)))))
    (fresh-line)
    (dotimes (i 10)
      (format t "~x " (read-memory mem (+ #x6000 i))))))

;; xD940 Crash on mem

;; 255 255 255 0 226 131 230 3 226
#+or
(time (run-system))

;; (dolist (i '(#x50 #x61 #x73 #x73 #x65 #x64))
;;   (format t "~a" (code-char i)))


;; nestest       C6BD - first bad nop

;; basic-01      E8D5
;; implied-02    E976
;; imm-03        E976
;; all-official  EC5B

;; (let* ((sys (init-system))
;;        (cart (system-cartridge sys))
;;        (mem (system-memory sys)))
;;   ;; (format t "~x" (cartridge-prg-rom cart))
;;   (format t "~x~%" (read-memory mem #xFFFC))
;;   (format t "~x~%" (read-memory mem #xFFFD)))
