(defpackage nesoteric.tests.addressing
  (:use :cl
        :nesoteric
        :nesoteric.cpu
        :nesoteric.memory
        :rove)
  (:local-nicknames (#:cpu #:nesoteric.cpu)))
(in-package :nesoteric.tests.addressing)

#+or
(rove:run-suite *package*)

#+or
(rove:run-test 'test-post-indexed-indirect)

(defparameter *cpu* (cpu::make-cpu))
(defparameter *mem* (make-memory))

(defun setup-test-memory ()
  (setf *cpu* (cpu::make-cpu)
        *mem* (make-memory)))

(deftest test-immediate-addressing
  (testing "immediate addressing mode"
    (setup-test-memory)
    (write-memory *mem* 0 #x42)          ; Write test value at PC
    (ok (= (read-memory *mem* (cpu::immediate *cpu* *mem*)) #x42) "should read the value at PC")
    (ok (= (cpu::cpu-program-counter *cpu*) 1) "should increment PC by 1")))

(deftest test-absolute-addressing
  (testing "absolute addressing mode"
    (setup-test-memory)
    (write-memory *mem* 0 #x34)          ; Low byte of base address
    (write-memory *mem* 1 #x07)          ; High byte of base address
    ;; Write test values
    (write-memory *mem* #x734 #x42)      ; Value at target address 0x0734
    (write-memory *mem* #x34 #xFF)       ; Wrong if high byte ignored
    (write-memory *mem* #x735 #xFF)      ; Wrong if off by one
    (ok (= (read-memory *mem* (cpu::absolute *cpu* *mem*)) #x42) "should read correct value using both address bytes")
    (ok (= (cpu::cpu-program-counter *cpu*) 2) "should increment PC by 2")))

(deftest test-zero-page
  (testing "zero page addressing mode"
    (setup-test-memory)
    (write-memory *mem* 0 #x42)          ; Address in zero page
    (write-memory *mem* #x42 #x37)       ; Value at zero page address
    (ok (= (read-memory *mem* (cpu::zero-page *cpu* *mem*)) #x37) "should read correct value from zero page")
    (ok (= (cpu::cpu-program-counter *cpu*) 1) "should increment PC by 1")))


(deftest test-absolute-x
  (testing "absolute-x addressing mode - normal case"
    (setup-test-memory)
    (setf (cpu::cpu-x-register *cpu*) #x02)
    (write-memory *mem* 0 #x34)          ; Low byte of base address
    (write-memory *mem* 1 #x07)          ; High byte of base address
    ;; Write test values
    (write-memory *mem* #x736 #x42)      ; Value at target (0x0734 + X)
    (write-memory *mem* #x36 #xFF)       ; Wrong if high byte ignored
    (ok (= (read-memory *mem* (cpu::absolute-x *cpu* *mem*)) #x42) "should read correct value using both address bytes")
    (ok (= (cpu::cpu-program-counter *cpu*) 2) "should increment PC by 2"))

  (testing "absolute-x addressing mode - page boundary crossing"
    (setup-test-memory)
    (setf (cpu::cpu-x-register *cpu*) #xFF)
    (write-memory *mem* 0 #x34)          ; Low byte of base address
    (write-memory *mem* 1 #x06)          ; High byte of base address
    ;; Write test values
    (write-memory *mem* #x733 #x42)      ; Value at target (0x0634 + 0xFF)
    (write-memory *mem* #x33 #xFF)       ; Wrong if high byte ignored
    (ok (= (read-memory *mem* (cpu::absolute-x *cpu* *mem*)) #x42) "should read correct value when crossing page boundary")
    (ok (= (cpu::cpu-program-counter *cpu*) 2) "should increment PC by 2")))

;; TODO Test that wrapping works ie if val is FF then adding to it should work
(deftest test-absolute-y
  (testing "absolute-y addressing mode - normal case"
    (setup-test-memory)
    (setf (cpu::cpu-y-register *cpu*) #x02)
    (write-memory *mem* 0 #x34)          ; Low byte of base address
    (write-memory *mem* 1 #x07)          ; High byte of base address
    ;; Write test values
    (write-memory *mem* #x736 #x42)      ; Value at target (0x0734 + Y)
    (write-memory *mem* #x36 #xFF)       ; Wrong if high byte ignored
    (ok (= (read-memory *mem* (cpu::absolute-y *cpu* *mem*)) #x42) "should read correct value using both address bytes")
    (ok (= (cpu::cpu-program-counter *cpu*) 2) "should increment PC by 2"))

  (testing "absolute-y addressing mode - page boundary crossing"
    (setup-test-memory)
    (setf (cpu::cpu-y-register *cpu*) #xFF)
    (write-memory *mem* 0 #x34)          ; Low byte of base address
    (write-memory *mem* 1 #x06)          ; High byte of base address
    ;; Write test values
    (write-memory *mem* #x733 #x42)      ; Value at target (0x0634 + 0xFF)
    (write-memory *mem* #x33 #xFF)       ; Wrong if high byte ignored
    (ok (= (read-memory *mem* (cpu::absolute-y *cpu* *mem*)) #X42) "should read correct value when crossing page boundary")
    (ok (= (cpu::cpu-program-counter *cpu*) 2) "should increment PC by 2")))

(deftest test-zero-page-x
  (testing "zero-page-x addressing mode - no wrap"
    (setup-test-memory)
    (setf (cpu::cpu-x-register *cpu*) #x02)
    (write-memory *mem* 0 #x42)          ; Zero page address
    (write-memory *mem* #x44 #x37)       ; Value at (zero page + X)
    (ok (= (read-memory *mem* (cpu::zero-page-x *cpu* *mem*)) #x37) "should read correct value from X-indexed zero page")
    (ok (= (cpu::cpu-program-counter *cpu*) 1) "should increment PC by 1"))

  (testing "zero-page-x addressing mode - with wrap"
    (setup-test-memory)
    (setf (cpu::cpu-x-register *cpu*) #x04)
    (write-memory *mem* 0 #xFE)          ; Zero page address
    (write-memory *mem* #x02 #x37)       ; Value at wrapped address (0xFE + 0x04 = 0x102 -> 0x02)
    (write-memory *mem* #x102 #xFF)      ; Wrong value if no wrapping occurs
    (ok (= (read-memory *mem* (cpu::zero-page-x *cpu* *mem*)) #x37) "should read correct wrapped value from X-indexed zero page")
    (ok (= (cpu::cpu-program-counter *cpu*) 1) "should increment PC by 1")))

(deftest test-zero-page-y
  (testing "zero-page-y addressing mode - no wrap"
    (setup-test-memory)
    (setf (cpu::cpu-y-register *cpu*) #x02)
    (write-memory *mem* 0 #x42)          ; Zero page address
    (write-memory *mem* #x44 #x37)       ; Value at (zero page + Y)
    (ok (= (read-memory *mem* (cpu::zero-page-y *cpu* *mem*)) #x37) "should read correct value from Y-indexed zero page")
    (ok (= (cpu::cpu-program-counter *cpu*) 1) "should increment PC by 1"))

  (testing "zero-page-y addressing mode - with wrap"
    (setup-test-memory)
    (setf (cpu::cpu-y-register *cpu*) #x04)
    (write-memory *mem* 0 #xFE)          ; Zero page address
    (write-memory *mem* #x02 #x37)       ; Value at wrapped address (0xFE + 0x04 = 0x102 -> 0x02)
    (write-memory *mem* #x102 #xFF)      ; Wrong value if no wrapping occurs
    (ok (= (read-memory *mem* (cpu::zero-page-y *cpu* *mem*)) #x37) "should read correct wrapped value from Y-indexed zero page")
    (ok (= (cpu::cpu-program-counter *cpu*) 1) "should increment PC by 1")))

(deftest test-indirect
  (testing "indirect addressing mode"
    (setup-test-memory)
    ;; Write pointer address
    (write-memory *mem* 0 #x34)          ; Low byte of pointer
    (write-memory *mem* 1 #x07)          ; High byte of pointer
    ;; Write target address at pointer location
    (write-memory *mem* #x734 #x78)      ; Low byte of target
    (write-memory *mem* #x735 #x06)      ; High byte of target (keeping in main memory)
    ;; Write test values
    (write-memory *mem* #x678 #xFF)      ; Wrong if bytes swapped
    (write-memory *mem* #x787 #xFF)      ; Wrong if bytes swapped other way
    (ok (= (cpu::indirect *cpu* *mem*) #x0678) "should read correct address from both pointer bytes")
    (ok (= (cpu::cpu-program-counter *cpu*) 2) "should increment PC by 2")))

(deftest test-pre-indexed-indirect
  (testing "pre-indexed-indirect addressing mode"
    (setup-test-memory)
    (setf (cpu::cpu-x-register *cpu*) #x02)
    ;; Write zero page address
    (write-memory *mem* 0 #x42)          ; Zero page address
    ;; Write target address at (zero page + X)
    (write-memory *mem* #x44 #x34)       ; Low byte of target at (ZP + X)
    (write-memory *mem* #x45 #x07)       ; High byte of target at (ZP + X)
    ;; Write test values at multiple addresses to verify correct addressing
    (write-memory *mem* #x734 #x42)      ; Expected address
    (write-memory *mem* #x34 #xFF)       ; Wrong if high byte ignored
    (write-memory *mem* #x735 #xFF)      ; Wrong if off by one
    (ok (= (read-memory *mem* (cpu::pre-indexed-indirect *cpu* *mem*)) #x42) "should read correct value using both address bytes")
    (ok (= (cpu::cpu-program-counter *cpu*) 1) "should increment PC by 1"))

  (testing "pre-indexed-indirect addressing mode with zero-page wrapping"
    (setup-test-memory)
    (setf (cpu::cpu-x-register *cpu*) #xFF)
    ;; Write zero page address
    (write-memory *mem* 0 #x42)          ; Zero page address
    ;; When #x42 + #xFF (255) = #x141, should wrap to #x41 in zero page
    ;; Write target address at wrapped location
    (write-memory *mem* #x41 #x34)       ; Low byte of target at wrapped (ZP + X)
    (write-memory *mem* #x42 #x07)       ; High byte of target at wrapped (ZP + X)
    ;; Write test values at multiple addresses
    (write-memory *mem* #x734 #x42)      ; Expected address
    (write-memory *mem* #x141 #xFF)      ; Wrong if no wrapping occurs
    (write-memory *mem* #x142 #xFF)      ; Wrong if no wrapping occurs
    (ok (= (read-memory *mem* (cpu::pre-indexed-indirect *cpu* *mem*)) #x42) "should wrap within zero page and read correct value")
    (ok (= (cpu::cpu-program-counter *cpu*) 1) "should increment PC by 1"))

  (testing "pre-indexed-indirect addressing mode with zero-page boundary read"
    (setup-test-memory)
    (setf (cpu::cpu-x-register *cpu*) #x02)
    (setf (cpu::cpu-program-counter *cpu*) #x100)
    ;; Write zero page address to fetch from
    (write-memory *mem* #x100 #xFD)      ; Zero page address + X will be 0xFF
    ;; Write target address crossing zero page boundary
    (write-memory *mem* #xFF #x34)       ; Low byte at end of zero page
    (write-memory *mem* #x00 #x07)       ; High byte wraps to start of zero page
    ;; Write test value
    (write-memory *mem* #x734 #x42)      ; Expected address
    (ok (= (read-memory *mem* (cpu::pre-indexed-indirect *cpu* *mem*)) #x42) "should correctly read address bytes across zero page boundary")
    (ok (= (cpu::cpu-program-counter *cpu*) #x101) "should increment PC by 1")))

(deftest test-post-indexed-indirect
  (testing "post-indexed-indirect addressing mode"
    (setup-test-memory)
    (setf (cpu::cpu-y-register *cpu*) #x02)
    ;; Write zero page address
    (write-memory *mem* 0 #x42)         ; Zero page address
    ;; Write base address at zero page
    (write-memory *mem* #x42 #x34)      ; Low byte of base address
    (write-memory *mem* #x43 #x07) ; High byte of base address, using 0x07 for max main memory
    ;; Write test values at multiple addresses
    (write-memory *mem* #x736 #x42)     ; Expected address (0x0734 + 2)
    (write-memory *mem* #x36 #xFF)      ; Wrong if high byte ignored
    (write-memory *mem* #x735 #xFF)     ; Wrong if off by one
    (ok (= (read-memory *mem* (cpu::post-indexed-indirect *cpu* *mem*)) #x42) "should read correct value using both address bytes")
    (ok (= (cpu::cpu-program-counter *cpu*) 1) "should increment PC by 1"))

  ;; TODO figure out why this is failing
  ;; remember I added the one below for bug with number overflowing
  ;; needed to wrap the final address with a logand
  (testing "post-indexed-indirect addressing mode with page boundary crossing"
    (setup-test-memory)
    (setf (cpu::cpu-y-register *cpu*) #x02)
    ;; Write zero page address
    (write-memory *mem* 0 #x42)         ; Zero page address
    ;; Write base address at zero page that will cross page when Y added
    (write-memory *mem* #x42 #xFF)   ; Low byte of base address
    (write-memory *mem* #x43 #x07)   ; High byte of base address
    ;; Write test value at page-crossed address
    (write-memory *mem* #x801 #x42)     ; Expected address (0x07FF + 2)
    (ok (= (read-memory *mem* (cpu::post-indexed-indirect *cpu* *mem*)) #x42) "should read correct value after page crossing")
    (ok (= (cpu::cpu-program-counter *cpu*) 1) "should increment PC by 1"))

  ;; bug
  (testing "post-indexed-indirect addressing mode with wrapping after y add"
    (setup-test-memory)
    (setf (cpu::cpu-y-register *cpu*) #x34)
    (write-memory *mem* 0    #x97)
    (write-memory *mem* #x97 #xFF)
    (write-memory *mem* #x33 #x12)
    (let ((mem (read-memory *mem* (cpu::post-indexed-indirect *cpu* *mem*))))
      (ok (= mem #x12) "should read a wrapped value"))
    (ok (= (cpu::cpu-program-counter *cpu*) 1) "should increment PC by 1")))

(deftest test-relative
  (testing "relative cpu addressing mode - range tests"
    (setup-test-memory)
    ;; Test maximum positive offset
    (write-memory *mem* 0 #x7F)          ; +127 (maximum positive)
    (ok (= (cpu::relative *cpu* *mem*) #x7F) "should handle maximum positive offset (+127)")
    (ok (= (cpu::cpu-program-counter *cpu*) 1) "should increment PC by 1")

    (setup-test-memory)
    ;; Test maximum negative offset
    (write-memory *mem* 0 #x80)          ; -128 (maximum negative)
    (ok (= (cpu::relative *cpu* *mem*) -128) "should handle maximum negative offset (-128)")
    (ok (= (cpu::cpu-program-counter *cpu*) 1) "should increment PC by 1")

    (setup-test-memory)
    ;; Test zero offset
    (write-memory *mem* 0 #x00)
    (ok (= (cpu::relative *cpu* *mem*) #x00) "should handle zero offset")
    (ok (= (cpu::cpu-program-counter *cpu*) 1) "should increment PC by 1")

    (setup-test-memory)
    ;; Test small positive offset
    (write-memory *mem* 0 #x01)          ; +1
    (ok (= (cpu::relative *cpu* *mem*) #x01) "should handle small positive offset")
    (ok (= (cpu::cpu-program-counter *cpu*) 1) "should increment PC by 1")

    (setup-test-memory)
    ;; Test small negative offset
    (write-memory *mem* 0 #xFF)          ; -1 in two's complement
    (ok (= (cpu::relative *cpu* *mem*) -1) "should handle small negative offset")
    (ok (= (cpu::cpu-program-counter *cpu*) 1) "should increment PC by 1")))
