(defpackage nesoteric/tests/addressing
  (:use :cl
        :nesoteric
        :nesoteric/cpu
        :rove)
  (:local-nicknames (#:cpu #:nesoteric/cpu)))
(in-package :nesoteric/tests/addressing)

#+or
(rove:run-suite *package*)

;; (rove:run-test 'test-immediate-addressing)

(defun setup-test-memory ()
  (setf cpu::*default-cpu* (cpu::make-cpu)))

(deftest test-immediate-addressing
  (testing "immediate addressing mode"
    (setup-test-memory)
    (cpu::write-memory 0 #x42)          ; Write test value at PC
    (ok (= (cpu::immediate) #x42) "should read the value at PC")
    (ok (= (cpu::cpu-program-counter cpu::*default-cpu*) 1) "should increment PC by 1")))

(deftest test-absolute-addressing
  (testing "absolute addressing mode"
    (setup-test-memory)
    (cpu::write-memory 0 #x34)          ; Low byte of base address
    (cpu::write-memory 1 #x07)          ; High byte of base address
    ;; Write test values
    (cpu::write-memory #x734 #x42)      ; Value at target address 0x0734
    (cpu::write-memory #x34 #xFF)       ; Wrong if high byte ignored
    (cpu::write-memory #x735 #xFF)      ; Wrong if off by one
    (ok (= (cpu::absolute) #x42) "should read correct value using both address bytes")
    (ok (= (cpu::cpu-program-counter cpu::*default-cpu*) 2) "should increment PC by 2")))

(deftest test-zero-page
  (testing "zero page addressing mode"
    (setup-test-memory)
    (cpu::write-memory 0 #x42)          ; Address in zero page
    (cpu::write-memory #x42 #x37)       ; Value at zero page address
    (ok (= (cpu::zero-page) #x37) "should read correct value from zero page")
    (ok (= (cpu::cpu-program-counter cpu::*default-cpu*) 1) "should increment PC by 1")))

(deftest test-absolute-x
  (testing "absolute-x addressing mode - normal case"
    (setup-test-memory)
    (setf (cpu::cpu-x-register cpu::*default-cpu*) #x02)
    (cpu::write-memory 0 #x34)          ; Low byte of base address
    (cpu::write-memory 1 #x07)          ; High byte of base address
    ;; Write test values
    (cpu::write-memory #x736 #x42)      ; Value at target (0x0734 + X)
    (cpu::write-memory #x36 #xFF)       ; Wrong if high byte ignored
    (ok (= (cpu::absolute-x) #x42) "should read correct value using both address bytes")
    (ok (= (cpu::cpu-program-counter cpu::*default-cpu*) 2) "should increment PC by 2"))

  (testing "absolute-x addressing mode - page boundary crossing"
    (setup-test-memory)
    (setf (cpu::cpu-x-register cpu::*default-cpu*) #xFF)
    (cpu::write-memory 0 #x34)          ; Low byte of base address
    (cpu::write-memory 1 #x06)          ; High byte of base address
    ;; Write test values
    (cpu::write-memory #x733 #x42)      ; Value at target (0x0634 + 0xFF)
    (cpu::write-memory #x33 #xFF)       ; Wrong if high byte ignored
    (ok (= (cpu::absolute-x) #x42) "should read correct value when crossing page boundary")
    (ok (= (cpu::cpu-program-counter cpu::*default-cpu*) 2) "should increment PC by 2")))

(deftest test-absolute-y
  (testing "absolute-y addressing mode - normal case"
    (setup-test-memory)
    (setf (cpu::cpu-y-register cpu::*default-cpu*) #x02)
    (cpu::write-memory 0 #x34)          ; Low byte of base address
    (cpu::write-memory 1 #x07)          ; High byte of base address
    ;; Write test values
    (cpu::write-memory #x736 #x42)      ; Value at target (0x0734 + Y)
    (cpu::write-memory #x36 #xFF)       ; Wrong if high byte ignored
    (ok (= (cpu::absolute-y) #x42) "should read correct value using both address bytes")
    (ok (= (cpu::cpu-program-counter cpu::*default-cpu*) 2) "should increment PC by 2"))

  (testing "absolute-y addressing mode - page boundary crossing"
    (setup-test-memory)
    (setf (cpu::cpu-y-register cpu::*default-cpu*) #xFF)
    (cpu::write-memory 0 #x34)          ; Low byte of base address
    (cpu::write-memory 1 #x06)          ; High byte of base address
    ;; Write test values
    (cpu::write-memory #x733 #x42)      ; Value at target (0x0634 + 0xFF)
    (cpu::write-memory #x33 #xFF)       ; Wrong if high byte ignored
    (ok (= (cpu::absolute-y) #x42) "should read correct value when crossing page boundary")
    (ok (= (cpu::cpu-program-counter cpu::*default-cpu*) 2) "should increment PC by 2")))

(deftest test-zero-page-x
  (testing "zero-page-x addressing mode - no wrap"
    (setup-test-memory)
    (setf (cpu::cpu-x-register cpu::*default-cpu*) #x02)
    (cpu::write-memory 0 #x42)          ; Zero page address
    (cpu::write-memory #x44 #x37)       ; Value at (zero page + X)
    (ok (= (cpu::zero-page-x) #x37) "should read correct value from X-indexed zero page")
    (ok (= (cpu::cpu-program-counter cpu::*default-cpu*) 1) "should increment PC by 1"))

  (testing "zero-page-x addressing mode - with wrap"
    (setup-test-memory)
    (setf (cpu::cpu-x-register cpu::*default-cpu*) #x04)
    (cpu::write-memory 0 #xFE)          ; Zero page address
    (cpu::write-memory #x02 #x37)       ; Value at wrapped address (0xFE + 0x04 = 0x102 -> 0x02)
    (cpu::write-memory #x102 #xFF)      ; Wrong value if no wrapping occurs
    (ok (= (cpu::zero-page-x) #x37) "should read correct wrapped value from X-indexed zero page")
    (ok (= (cpu::cpu-program-counter cpu::*default-cpu*) 1) "should increment PC by 1")))

(deftest test-zero-page-y
  (testing "zero-page-y addressing mode - no wrap"
    (setup-test-memory)
    (setf (cpu::cpu-y-register cpu::*default-cpu*) #x02)
    (cpu::write-memory 0 #x42)          ; Zero page address
    (cpu::write-memory #x44 #x37)       ; Value at (zero page + Y)
    (ok (= (cpu::zero-page-y) #x37) "should read correct value from Y-indexed zero page")
    (ok (= (cpu::cpu-program-counter cpu::*default-cpu*) 1) "should increment PC by 1"))

  (testing "zero-page-y addressing mode - with wrap"
    (setup-test-memory)
    (setf (cpu::cpu-y-register cpu::*default-cpu*) #x04)
    (cpu::write-memory 0 #xFE)          ; Zero page address
    (cpu::write-memory #x02 #x37)       ; Value at wrapped address (0xFE + 0x04 = 0x102 -> 0x02)
    (cpu::write-memory #x102 #xFF)      ; Wrong value if no wrapping occurs
    (ok (= (cpu::zero-page-y) #x37) "should read correct wrapped value from Y-indexed zero page")
    (ok (= (cpu::cpu-program-counter cpu::*default-cpu*) 1) "should increment PC by 1")))

(deftest test-indirect
  (testing "indirect addressing mode"
    (setup-test-memory)
    ;; Write pointer address
    (cpu::write-memory 0 #x34)          ; Low byte of pointer
    (cpu::write-memory 1 #x07)          ; High byte of pointer
    ;; Write target address at pointer location
    (cpu::write-memory #x734 #x78)      ; Low byte of target
    (cpu::write-memory #x735 #x06)      ; High byte of target (keeping in main memory)
    ;; Write test values
    (cpu::write-memory #x678 #xFF)      ; Wrong if bytes swapped
    (cpu::write-memory #x787 #xFF)      ; Wrong if bytes swapped other way
    (ok (= (cpu::indirect) #x0678) "should read correct address from both pointer bytes")
    (ok (= (cpu::cpu-program-counter cpu::*default-cpu*) 2) "should increment PC by 2")))

(deftest test-pre-indexed-indirect
  (testing "pre-indexed-indirect addressing mode"
    (setup-test-memory)
    (setf (cpu::cpu-x-register cpu::*default-cpu*) #x02)
    ;; Write zero page address
    (cpu::write-memory 0 #x42)          ; Zero page address
    ;; Write target address at (zero page + X)
    (cpu::write-memory #x44 #x34)       ; Low byte of target at (ZP + X)
    (cpu::write-memory #x45 #x07)       ; High byte of target at (ZP + X)
    ;; Write test values at multiple addresses to verify correct addressing
    (cpu::write-memory #x734 #x42)      ; Expected address
    (cpu::write-memory #x34 #xFF)       ; Wrong if high byte ignored
    (cpu::write-memory #x735 #xFF)      ; Wrong if off by one
    (ok (= (cpu::pre-indexed-indirect) #x42) "should read correct value using both address bytes")
    (ok (= (cpu::cpu-program-counter cpu::*default-cpu*) 1) "should increment PC by 1"))

  (testing "pre-indexed-indirect addressing mode with zero-page wrapping"
    (setup-test-memory)
    (setf (cpu::cpu-x-register cpu::*default-cpu*) #xFF)
    ;; Write zero page address
    (cpu::write-memory 0 #x42)          ; Zero page address
    ;; When #x42 + #xFF (255) = #x141, should wrap to #x41 in zero page
    ;; Write target address at wrapped location
    (cpu::write-memory #x41 #x34)       ; Low byte of target at wrapped (ZP + X)
    (cpu::write-memory #x42 #x07)       ; High byte of target at wrapped (ZP + X)
    ;; Write test values at multiple addresses
    (cpu::write-memory #x734 #x42)      ; Expected address
    (cpu::write-memory #x141 #xFF)      ; Wrong if no wrapping occurs
    (cpu::write-memory #x142 #xFF)      ; Wrong if no wrapping occurs
    (ok (= (cpu::pre-indexed-indirect) #x42) "should wrap within zero page and read correct value")
    (ok (= (cpu::cpu-program-counter cpu::*default-cpu*) 1) "should increment PC by 1"))

  (testing "pre-indexed-indirect addressing mode with zero-page boundary read"
    (setup-test-memory)
    (setf (cpu::cpu-x-register cpu::*default-cpu*) #x02)
    (setf (cpu::cpu-program-counter cpu::*default-cpu*) #x100)
    ;; Write zero page address to fetch from
    (cpu::write-memory #x100 #xFD)      ; Zero page address + X will be 0xFF
    ;; Write target address crossing zero page boundary
    (cpu::write-memory #xFF #x34)       ; Low byte at end of zero page
    (cpu::write-memory #x00 #x07)       ; High byte wraps to start of zero page
    ;; Write test value
    (cpu::write-memory #x734 #x42)      ; Expected address
    (ok (= (cpu::pre-indexed-indirect) #x42) "should correctly read address bytes across zero page boundary")
    (ok (= (cpu::cpu-program-counter cpu::*default-cpu*) #x101) "should increment PC by 1")) )

(deftest test-post-indexed-indirect
  (testing "post-indexed-indirect addressing mode"
    (setup-test-memory)
    (setf (cpu::cpu-y-register cpu::*default-cpu*) #x02)
    ;; Write zero page address
    (cpu::write-memory 0 #x42)          ; Zero page address
    ;; Write base address at zero page
    (cpu::write-memory #x42 #x34)       ; Low byte of base address
    (cpu::write-memory #x43 #x07)       ; High byte of base address, using 0x07 for max main memory
    ;; Write test values at multiple addresses
    (cpu::write-memory #x736 #x42)      ; Expected address (0x0734 + 2)
    (cpu::write-memory #x36 #xFF)       ; Wrong if high byte ignored
    (cpu::write-memory #x735 #xFF)      ; Wrong if off by one
    (ok (= (cpu::post-indexed-indirect) #x42) "should read correct value using both address bytes")
    (ok (= (cpu::cpu-program-counter cpu::*default-cpu*) 1) "should increment PC by 1"))

  (testing "post-indexed-indirect addressing mode with page boundary crossing"
    (setup-test-memory)
    (setf (cpu::cpu-y-register cpu::*default-cpu*) #x02)
    ;; Write zero page address
    (cpu::write-memory 0 #x42)          ; Zero page address
    ;; Write base address at zero page that will cross page when Y added
    (cpu::write-memory #x42 #xFF)       ; Low byte of base address
    (cpu::write-memory #x43 #x07)       ; High byte of base address
    ;; Write test value at page-crossed address
    (cpu::write-memory #x801 #x42)      ; Expected address (0x07FF + 2)
    (ok (= (cpu::post-indexed-indirect) #x42) "should read correct value after page crossing")
    (ok (= (cpu::cpu-program-counter cpu::*default-cpu*) 1) "should increment PC by 1")))

(deftest test-relative
  (testing "relative addressing mode - range tests"
    (setup-test-memory)
    ;; Test maximum positive offset
    (cpu::write-memory 0 #x7F)          ; +127 (maximum positive)
    (ok (= (cpu::relative) #x7F) "should handle maximum positive offset (+127)")
    (ok (= (cpu::cpu-program-counter cpu::*default-cpu*) 1) "should increment PC by 1")

    (setup-test-memory)
    ;; Test maximum negative offset
    (cpu::write-memory 0 #x80)          ; -128 (maximum negative)
    (ok (= (cpu::relative) #x80) "should handle maximum negative offset (-128)")
    (ok (= (cpu::cpu-program-counter cpu::*default-cpu*) 1) "should increment PC by 1")

    (setup-test-memory)
    ;; Test zero offset
    (cpu::write-memory 0 #x00)
    (ok (= (cpu::relative) #x00) "should handle zero offset")
    (ok (= (cpu::cpu-program-counter cpu::*default-cpu*) 1) "should increment PC by 1")

    (setup-test-memory)
    ;; Test small positive offset
    (cpu::write-memory 0 #x01)          ; +1
    (ok (= (cpu::relative) #x01) "should handle small positive offset")
    (ok (= (cpu::cpu-program-counter cpu::*default-cpu*) 1) "should increment PC by 1")

    (setup-test-memory)
    ;; Test small negative offset
    (cpu::write-memory 0 #xFF)          ; -1 in two's complement
    (ok (= (cpu::relative) #xFF) "should handle small negative offset")
    (ok (= (cpu::cpu-program-counter cpu::*default-cpu*) 1) "should increment PC by 1"))

  (testing "relative addressing mode - PC relative jumps"
    (setup-test-memory)
    (setf (cpu::cpu-program-counter cpu::*default-cpu*) #x200)
    ;; Test forward branch that crosses a page
    (cpu::write-memory #x200 #x7F)      ; +127 from 0x201 should go to 0x280
    (ok (= (cpu::relative) #x7F) "should calculate forward branch target")
    (ok (= (cpu::cpu-program-counter cpu::*default-cpu*) #x201) "should increment PC by 1")

    (setup-test-memory)
    (setf (cpu::cpu-program-counter cpu::*default-cpu*) #x280)
    ;; Test backward branch that crosses a page
    (cpu::write-memory #x280 #x80)      ; -128 from 0x281 should go to 0x201
    (ok (= (cpu::relative) #x80) "should calculate backward branch target")
    (ok (= (cpu::cpu-program-counter cpu::*default-cpu*) #x281) "should increment PC by 1"))

  (testing "relative addressing mode - range tests from middle of memory"
    (setup-test-memory)
    (setf (cpu::cpu-program-counter cpu::*default-cpu*) #x200)
    ;; Test maximum positive offset from mid-memory
    (cpu::write-memory #x200 #x7F)      ; +127 (maximum positive)
    (ok (= (cpu::relative) #x7F) "should handle maximum positive offset (+127)")
    (ok (= (cpu::cpu-program-counter cpu::*default-cpu*) #x201) "should increment PC by 1")

    (setup-test-memory)
    (setf (cpu::cpu-program-counter cpu::*default-cpu*) #x300)
    ;; Test maximum negative offset from different location
    (cpu::write-memory #x300 #x80)      ; -128 (maximum negative)
    (ok (= (cpu::relative) #x80) "should handle maximum negative offset (-128)")
    (ok (= (cpu::cpu-program-counter cpu::*default-cpu*) #x301) "should increment PC by 1")

    (setup-test-memory)
    (setf (cpu::cpu-program-counter cpu::*default-cpu*) #x150)
    ;; Test zero offset from another location
    (cpu::write-memory #x150 #x00)
    (ok (= (cpu::relative) #x00) "should handle zero offset")
    (ok (= (cpu::cpu-program-counter cpu::*default-cpu*) #x151) "should increment PC by 1")

    (setup-test-memory)
    (setf (cpu::cpu-program-counter cpu::*default-cpu*) #x400)
    ;; Test small positive offset
    (cpu::write-memory #x400 #x01)      ; +1
    (ok (= (cpu::relative) #x01) "should handle small positive offset")
    (ok (= (cpu::cpu-program-counter cpu::*default-cpu*) #x401) "should increment PC by 1")

    (setup-test-memory)
    (setf (cpu::cpu-program-counter cpu::*default-cpu*) #x350)
    ;; Test small negative offset
    (cpu::write-memory #x350 #xFF)      ; -1 in two's complement
    (ok (= (cpu::relative) #xFF) "should handle small negative offset")
    (ok (= (cpu::cpu-program-counter cpu::*default-cpu*) #x351) "should increment PC by 1")))
