(defpackage nesoteric/tests/instructions
  (:use :cl
        :nesoteric
        :nesoteric/cpu
        :rove)
  (:local-nicknames (#:cpu #:nesoteric/cpu)))
(in-package :nesoteric/tests/instructions)

(defun setup-test-cpu ()
  (setf cpu::*default-cpu* (cpu::make-cpu)))

(deftest test-adc
  (testing "adc - basic addition"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #X20)
    (cpu::write-memory 0 #X69)  ; ADC immediate
    (cpu::write-memory 1 #X42)
    (cpu::cpu-step)
    (ok (= #X62 (cpu::cpu-accumulator cpu::*default-cpu*)) "Accumulator should contain #$20 + #$42"))

  (testing "adc - addition with carry set"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #X20)
    (cpu::set-flag :carry 1)
    (cpu::write-memory 0 #X69)
    (cpu::write-memory 1 #X42)
    (cpu::cpu-step)
    (ok (= #X63 (cpu::cpu-accumulator cpu::*default-cpu*)) "Accumulator should contain #$20 + #$42 + carry"))

  (testing "adc - carry flag behavior"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #XFF)
    (cpu::write-memory 0 #X69)
    (cpu::write-memory 1 #X01)
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :carry)) "Carry flag should be set when result exceeds #$FF")
    (ok (= #X00 (cpu::cpu-accumulator cpu::*default-cpu*)) "Accumulator should wrap to #$00 when sum exceeds #$FF"))

  (testing "adc - zero flag behavior"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #X00)
    (cpu::write-memory 0 #X69)
    (cpu::write-memory 1 #X00)
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be set when result is #$00"))

  (testing "adc - negative flag behavior"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #X50)
    (cpu::write-memory 0 #X69)
    (cpu::write-memory 1 #X90)
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be set when result has bit 7 set")))

(deftest test-and
  (testing "and - basic operation"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #b11110000)
    (cpu::write-memory 0 #X29)  ; AND immediate
    (cpu::write-memory 1 #b10101010)
    (cpu::cpu-step)
    (ok (= #b10100000 (cpu::cpu-accumulator cpu::*default-cpu*)) "Accumulator should contain F0 AND AA"))

  (testing "and - zero flag behavior"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #b11110000)
    (cpu::write-memory 0 #X29)
    (cpu::write-memory 1 #b00001111)
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be set when result is 0"))

  (testing "and - negative flag behavior"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #xFF)
    (cpu::write-memory 0 #X29)
    (cpu::write-memory 1 #b10000000)
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be set when result has bit 7 set")))

(deftest test-asl
  (testing "asl - basic shift operation"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #b01010101)
    (cpu::write-memory 0 #x0A)          ; ASL accumulator
    (cpu::cpu-step)
    (ok (= #b10101010 (cpu::cpu-accumulator cpu::*default-cpu*)) "Should shift all bits left by one position"))

  (testing "asl - carry flag behavior"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #b10000000)
    (cpu::write-memory 0 #x0A)
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :carry)) "Carry flag should contain the old bit 7")
    (ok (= #b00000000 (cpu::cpu-accumulator cpu::*default-cpu*)) "Should shift bit 7 out into carry"))

  (testing "asl - zero flag behavior"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #b10000000)
    (cpu::write-memory 0 #x0A)
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be set when result is 0"))

  (testing "asl - negative flag behavior"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #b01000000)
    (cpu::write-memory 0 #x0A)
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be set when result has bit 7 set")))

(deftest test-bcc
  (testing "bcc - branch when carry clear"
    (setup-test-cpu)
    (cpu::set-flag :carry 0)    ; Clear carry flag
    (cpu::write-memory 0 #x90)  ; BCC
    (cpu::write-memory 1 #x05)  ; Branch forward 5 bytes
    (cpu::cpu-step)
    (ok (= #x07 (cpu::cpu-program-counter cpu::*default-cpu*)) "Program counter should advance by offset + 2 when carry clear"))

  (testing "bcc - no branch when carry set"
    (setup-test-cpu)
    (cpu::set-flag :carry 1)    ; Set carry flag
    (cpu::write-memory 0 #x90)  ; BCC
    (cpu::write-memory 1 #x05)  ; Branch offset (should be ignored)
    (cpu::cpu-step)
    (ok (= #x02 (cpu::cpu-program-counter cpu::*default-cpu*)) "Program counter should only advance by 2 when carry set"))

  (testing "bcc - negative branch offset"
    (setup-test-cpu)
    (cpu::set-flag :carry 0)    ; Clear carry flag
    (cpu::write-memory 0 #x90)  ; BCC
    (cpu::write-memory 1 #xFA)  ; Branch backward 6 bytes (-6 in two's complement)
    (cpu::cpu-step)
    (ok (= #xFFFC (cpu::cpu-program-counter cpu::*default-cpu*)) "Program counter should subtract offset when branch taken with negative offset")))

(deftest test-bcs
  (testing "bcs - branch when carry set"
    (setup-test-cpu)
    (cpu::set-flag :carry 1)  ; Set carry flag
    (cpu::write-memory 0 #xB0)  ; BCS
    (cpu::write-memory 1 #x05)  ; Branch forward 5 bytes
    (cpu::cpu-step)
    (ok (= #x07 (cpu::cpu-program-counter cpu::*default-cpu*)) "Program counter should advance by offset + 2 when carry set"))

  (testing "bcs - no branch when carry clear"
    (setup-test-cpu)
    (cpu::set-flag :carry 0)  ; Clear carry flag
    (cpu::write-memory 0 #xB0)  ; BCS
    (cpu::write-memory 1 #x05)  ; Branch offset (should be ignored)
    (cpu::cpu-step)
    (ok (= #x02 (cpu::cpu-program-counter cpu::*default-cpu*)) "Program counter should only advance by 2 when carry clear"))

  (testing "bcs - negative branch offset"
    (setup-test-cpu)
    (cpu::set-flag :carry 1)  ; Set carry flag
    (cpu::write-memory 0 #xB0)  ; BCS
    (cpu::write-memory 1 #xFA)  ; Branch backward 6 bytes (-6 in two's complement)
    (cpu::cpu-step)
    (ok (= #xFFFC (cpu::cpu-program-counter cpu::*default-cpu*)) "Program counter should subtract offset when branch taken with negative offset")))

(deftest test-beq
  (testing "beq - branch when zero flag set"
    (setup-test-cpu)
    (cpu::set-flag :zero 1)  ; Set zero flag
    (cpu::write-memory 0 #xF0)  ; BEQ
    (cpu::write-memory 1 #x05)  ; Branch forward 5 bytes
    (cpu::cpu-step)
    (ok (= #x07 (cpu::cpu-program-counter cpu::*default-cpu*)) "Program counter should advance by offset + 2 when zero set"))

  (testing "beq - no branch when zero flag clear"
    (setup-test-cpu)
    (cpu::set-flag :zero 0)  ; Clear zero flag
    (cpu::write-memory 0 #xF0)  ; BEQ
    (cpu::write-memory 1 #x05)  ; Branch offset (should be ignored)
    (cpu::cpu-step)
    (ok (= #x02 (cpu::cpu-program-counter cpu::*default-cpu*)) "Program counter should only advance by 2 when zero clear"))

  (testing "beq - negative branch offset"
    (setup-test-cpu)
    (cpu::set-flag :zero 1)  ; Set zero flag
    (cpu::write-memory 0 #xF0)  ; BEQ
    (cpu::write-memory 1 #xFA)  ; Branch backward 6 bytes (-6 in two's complement)
    (cpu::cpu-step)
    (ok (= #xFFFC (cpu::cpu-program-counter cpu::*default-cpu*)) "Program counter should subtract offset when branch taken with negative offset")))

(deftest test-bit
  (testing "bit - zero flag behavior with AND result"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #b10000000)
    (cpu::write-memory 0 #x24)  ; BIT zero page
    (cpu::write-memory 1 #x50)  ; Zero page address
    (cpu::write-memory #x50 #b00000001)  ; Memory value
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be set when accumulator AND memory is zero"))

  (testing "bit - zero flag clear when bits match"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #b00000001)
    (cpu::write-memory 0 #x24)  ; BIT zero page
    (cpu::write-memory 1 #x50)
    (cpu::write-memory #x50 #b00000001)
    (cpu::cpu-step)
    (ok (= 0 (cpu::get-flag :zero)) "Zero flag should be clear when accumulator AND memory is non-zero"))

  (testing "bit - negative flag from memory bit 7"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #b00000001)
    (cpu::write-memory 0 #x24)  ; BIT zero page
    (cpu::write-memory 1 #x50)
    (cpu::write-memory #x50 #b10000000)  ; Bit 7 set in memory
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be set when memory bit 7 is set"))

  (testing "bit - overflow flag from memory bit 6"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #b00000001)
    (cpu::write-memory 0 #x24)  ; BIT zero page
    (cpu::write-memory 1 #x50)
    (cpu::write-memory #x50 #b01000000)  ; Bit 6 set in memory
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :overflow)) "Overflow flag should be set when memory bit 6 is set")))

(deftest test-bmi
  (testing "bmi - branch when negative flag set"
    (setup-test-cpu)
    (cpu::set-flag :negative 1)  ; Set negative flag
    (cpu::write-memory 0 #x30)  ; BMI
    (cpu::write-memory 1 #x05)  ; Branch forward 5 bytes
    (cpu::cpu-step)
    (ok (= #x07 (cpu::cpu-program-counter cpu::*default-cpu*)) "Program counter should advance by offset + 2 when negative set"))

  (testing "bmi - no branch when negative flag clear"
    (setup-test-cpu)
    (cpu::set-flag :negative 0)  ; Clear negative flag
    (cpu::write-memory 0 #x30)  ; BMI
    (cpu::write-memory 1 #x05)  ; Branch offset (should be ignored)
    (cpu::cpu-step)
    (ok (= #x02 (cpu::cpu-program-counter cpu::*default-cpu*)) "Program counter should only advance by 2 when negative clear"))

  (testing "bmi - negative branch offset"
    (setup-test-cpu)
    (cpu::set-flag :negative 1)  ; Set negative flag
    (cpu::write-memory 0 #x30)  ; BMI
    (cpu::write-memory 1 #xFA)  ; Branch backward 6 bytes (-6 in two's complement)
    (cpu::cpu-step)
    (ok (= #xFFFC (cpu::cpu-program-counter cpu::*default-cpu*)) "Program counter should subtract offset when branch taken with negative offset")))

(deftest test-bne
  (testing "bne - branch when zero flag clear"
    (setup-test-cpu)
    (cpu::set-flag :zero 0)  ; Clear zero flag
    (cpu::write-memory 0 #xD0)  ; BNE
    (cpu::write-memory 1 #x05)  ; Branch forward 5 bytes
    (cpu::cpu-step)
    (ok (= #x07 (cpu::cpu-program-counter cpu::*default-cpu*)) "Program counter should advance by offset + 2 when zero clear"))

  (testing "bne - no branch when zero flag set"
    (setup-test-cpu)
    (cpu::set-flag :zero 1)  ; Set zero flag
    (cpu::write-memory 0 #xD0)  ; BNE
    (cpu::write-memory 1 #x05)  ; Branch offset (should be ignored)
    (cpu::cpu-step)
    (ok (= #x02 (cpu::cpu-program-counter cpu::*default-cpu*)) "Program counter should only advance by 2 when zero set"))

  (testing "bne - negative branch offset"
    (setup-test-cpu)
    (cpu::set-flag :zero 0)  ; Clear zero flag
    (cpu::write-memory 0 #xD0)  ; BNE
    (cpu::write-memory 1 #xFA)  ; Branch backward 6 bytes (-6 in two's complement)
    (cpu::cpu-step)
    (ok (= #xFFFC (cpu::cpu-program-counter cpu::*default-cpu*)) "Program counter should subtract offset when branch taken with negative offset")))

(deftest test-bpl
  (testing "bpl - branch when negative flag clear"
    (setup-test-cpu)
    (cpu::set-flag :negative 0)  ; Clear negative flag
    (cpu::write-memory 0 #x10)  ; BPL
    (cpu::write-memory 1 #x05)  ; Branch forward 5 bytes
    (cpu::cpu-step)
    (ok (= #x07 (cpu::cpu-program-counter cpu::*default-cpu*)) "Program counter should advance by offset + 2 when negative clear"))

  (testing "bpl - no branch when negative flag set"
    (setup-test-cpu)
    (cpu::set-flag :negative 1)  ; Set negative flag
    (cpu::write-memory 0 #x10)  ; BPL
    (cpu::write-memory 1 #x05)  ; Branch offset (should be ignored)
    (cpu::cpu-step)
    (ok (= #x02 (cpu::cpu-program-counter cpu::*default-cpu*)) "Program counter should only advance by 2 when negative set"))

  (testing "bpl - negative branch offset"
    (setup-test-cpu)
    (cpu::set-flag :negative 0)  ; Clear negative flag
    (cpu::write-memory 0 #x10)  ; BPL
    (cpu::write-memory 1 #xFA)  ; Branch backward 6 bytes (-6 in two's complement)
    (cpu::cpu-step)
    (ok (= #xFFFC (cpu::cpu-program-counter cpu::*default-cpu*)) "Program counter should subtract offset when branch taken with negative offset")))

(deftest test-brk
  (fail "BRK Not Implemented."))

(deftest test-bvc
  (testing "bvc - branch when overflow flag clear"
    (setup-test-cpu)
    (cpu::set-flag :overflow 0)  ; Clear overflow flag
    (cpu::write-memory 0 #x50)  ; BVC
    (cpu::write-memory 1 #x05)  ; Branch forward 5 bytes
    (cpu::cpu-step)
    (ok (= #x07 (cpu::cpu-program-counter cpu::*default-cpu*)) "Program counter should advance by offset + 2 when overflow clear"))

  (testing "bvc - no branch when overflow flag set"
    (setup-test-cpu)
    (cpu::set-flag :overflow 1)  ; Set overflow flag
    (cpu::write-memory 0 #x50)  ; BVC
    (cpu::write-memory 1 #x05)  ; Branch offset (should be ignored)
    (cpu::cpu-step)
    (ok (= #x02 (cpu::cpu-program-counter cpu::*default-cpu*)) "Program counter should only advance by 2 when overflow set"))

  (testing "bvc - negative branch offset"
    (setup-test-cpu)
    (cpu::set-flag :overflow 0)  ; Clear overflow flag
    (cpu::write-memory 0 #x50)  ; BVC
    (cpu::write-memory 1 #xFA)  ; Branch backward 6 bytes (-6 in two's complement)
    (cpu::cpu-step)
    (ok (= #xFFFC (cpu::cpu-program-counter cpu::*default-cpu*)) "Program counter should subtract offset when branch taken with negative offset")))


(deftest test-bvs
  (testing "bvs - branch when overflow flag set"
    (setup-test-cpu)
    (cpu::set-flag :overflow 1)  ; Set overflow flag
    (cpu::write-memory 0 #x70)  ; BVS
    (cpu::write-memory 1 #x05)  ; Branch forward 5 bytes
    (cpu::cpu-step)
    (ok (= #x07 (cpu::cpu-program-counter cpu::*default-cpu*)) "Program counter should advance by offset + 2 when overflow set"))

  (testing "bvs - no branch when overflow flag clear"
    (setup-test-cpu)
    (cpu::set-flag :overflow 0)  ; Clear overflow flag
    (cpu::write-memory 0 #x70)  ; BVS
    (cpu::write-memory 1 #x05)  ; Branch offset (should be ignored)
    (cpu::cpu-step)
    (ok (= #x02 (cpu::cpu-program-counter cpu::*default-cpu*)) "Program counter should only advance by 2 when overflow clear"))

  (testing "bvs - negative branch offset"
    (setup-test-cpu)
    (cpu::set-flag :overflow 1)  ; Set overflow flag
    (cpu::write-memory 0 #x70)  ; BVS
    (cpu::write-memory 1 #xFA)  ; Branch backward 6 bytes (-6 in two's complement)
    (cpu::cpu-step)
    (ok (= #xFFFC (cpu::cpu-program-counter cpu::*default-cpu*)) "Program counter should subtract offset when branch taken with negative offset")))

(deftest test-clc
  (testing "clc - clear carry flag when set"
    (setup-test-cpu)
    (cpu::set-flag :carry 1)  ; Set carry flag
    (cpu::write-memory 0 #x18)  ; CLC
    (cpu::cpu-step)
    (ok (= 0 (cpu::get-flag :carry)) "Carry flag should be cleared"))

  (testing "clc - clear carry flag when already clear"
    (setup-test-cpu)
    (cpu::set-flag :carry 0)  ; Clear carry flag
    (cpu::write-memory 0 #x18)  ; CLC
    (cpu::cpu-step)
    (ok (= 0 (cpu::get-flag :carry)) "Carry flag should remain cleared"))

  (testing "clc - doesn't affect other flags"
    (setup-test-cpu)
    (cpu::set-flag :zero 1)
    (cpu::set-flag :overflow 1)
    (cpu::set-flag :negative 1)
    (cpu::set-flag :carry 1)
    (cpu::write-memory 0 #x18)  ; CLC
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be unchanged")
    (ok (= 1 (cpu::get-flag :overflow)) "Overflow flag should be unchanged")
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be unchanged")))

(deftest test-cld
  (testing "cld - clear decimal flag when set"
    (setup-test-cpu)
    (cpu::set-flag :decimal 1)  ; Set decimal flag
    (cpu::write-memory 0 #xD8)  ; CLD
    (cpu::cpu-step)
    (ok (= 0 (cpu::get-flag :decimal)) "Decimal flag should be cleared"))

  (testing "cld - clear decimal flag when already clear"
    (setup-test-cpu)
    (cpu::set-flag :decimal 0)  ; Clear decimal flag
    (cpu::write-memory 0 #xD8)  ; CLD
    (cpu::cpu-step)
    (ok (= 0 (cpu::get-flag :decimal)) "Decimal flag should remain cleared"))

  (testing "cld - doesn't affect other flags"
    (setup-test-cpu)
    (cpu::set-flag :zero 1)
    (cpu::set-flag :overflow 1)
    (cpu::set-flag :negative 1)
    (cpu::set-flag :carry 1)
    (cpu::set-flag :decimal 1)
    (cpu::write-memory 0 #xD8)  ; CLD
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be unchanged")
    (ok (= 1 (cpu::get-flag :overflow)) "Overflow flag should be unchanged")
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be unchanged")
    (ok (= 1 (cpu::get-flag :carry)) "Carry flag should be unchanged")))

(deftest test-cli
  (testing "cli - clear interrupt disable flag when set"
    (setup-test-cpu)
    (cpu::set-flag :interrupt 1)  ; Set interrupt flag
    (cpu::write-memory 0 #x58)  ; CLI
    (cpu::cpu-step)
    (ok (= 0 (cpu::get-flag :interrupt)) "Interrupt flag should be cleared"))

  (testing "cli - clear interrupt flag when already clear"
    (setup-test-cpu)
    (cpu::set-flag :interrupt 0)  ; Clear interrupt flag
    (cpu::write-memory 0 #x58)  ; CLI
    (cpu::cpu-step)
    (ok (= 0 (cpu::get-flag :interrupt)) "Interrupt flag should remain cleared"))

  (testing "cli - doesn't affect other flags"
    (setup-test-cpu)
    (cpu::set-flag :zero 1)
    (cpu::set-flag :overflow 1)
    (cpu::set-flag :negative 1)
    (cpu::set-flag :carry 1)
    (cpu::set-flag :decimal 1)
    (cpu::set-flag :interrupt 1)
    (cpu::write-memory 0 #x58)  ; CLI
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be unchanged")
    (ok (= 1 (cpu::get-flag :overflow)) "Overflow flag should be unchanged")
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be unchanged")
    (ok (= 1 (cpu::get-flag :carry)) "Carry flag should be unchanged")
    (ok (= 1 (cpu::get-flag :decimal)) "Decimal flag should be unchanged")))

(deftest test-clv
  (testing "clv - clear overflow flag when set"
    (setup-test-cpu)
    (cpu::set-flag :overflow 1)  ; Set overflow flag
    (cpu::write-memory 0 #xB8)  ; CLV
    (cpu::cpu-step)
    (ok (= 0 (cpu::get-flag :overflow)) "Overflow flag should be cleared"))

  (testing "clv - clear overflow flag when already clear"
    (setup-test-cpu)
    (cpu::set-flag :overflow 0)  ; Clear overflow flag
    (cpu::write-memory 0 #xB8)  ; CLV
    (cpu::cpu-step)
    (ok (= 0 (cpu::get-flag :overflow)) "Overflow flag should remain cleared"))

  (testing "clv - doesn't affect other flags"
    (setup-test-cpu)
    (cpu::set-flag :zero 1)
    (cpu::set-flag :negative 1)
    (cpu::set-flag :carry 1)
    (cpu::set-flag :decimal 1)
    (cpu::set-flag :interrupt 1)
    (cpu::set-flag :overflow 1)
    (cpu::write-memory 0 #xB8)  ; CLV
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be unchanged")
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be unchanged")
    (ok (= 1 (cpu::get-flag :carry)) "Carry flag should be unchanged")
    (ok (= 1 (cpu::get-flag :decimal)) "Decimal flag should be unchanged")
    (ok (= 1 (cpu::get-flag :interrupt)) "Interrupt flag should be unchanged")))

(deftest test-cmp
  (testing "cmp - zero flag when accumulator equals memory"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #x42)
    (cpu::write-memory 0 #xC9)  ; CMP immediate
    (cpu::write-memory 1 #x42)
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be set when values are equal")
    (ok (= 1 (cpu::get-flag :carry)) "Carry flag should be set when A >= M"))

  (testing "cmp - carry flag when accumulator greater than memory"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #x42)
    (cpu::write-memory 0 #xC9)  ; CMP immediate
    (cpu::write-memory 1 #x40)
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :carry)) "Carry flag should be set when A >= M")
    (ok (= 0 (cpu::get-flag :zero)) "Zero flag should be clear when values are not equal"))

  (testing "cmp - negative flag when result has bit 7 set"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #x40)
    (cpu::write-memory 0 #xC9)  ; CMP immediate
    (cpu::write-memory 1 #xC0)  ; Results in #x80 (negative)
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be set when result has bit 7 set")
    (ok (= 0 (cpu::get-flag :carry)) "Carry flag should be clear when A < M"))

  (testing "cmp - verify accumulator unchanged"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #x42)
    (cpu::write-memory 0 #xC9)  ; CMP immediate
    (cpu::write-memory 1 #x40)
    (cpu::cpu-step)
    (ok (= #x42 (cpu::cpu-accumulator cpu::*default-cpu*)) "Accumulator should remain unchanged")))

(deftest test-cpx
  (testing "cpx - zero flag when x register equals memory"
    (setup-test-cpu)
    (setf (cpu::cpu-x-register cpu::*default-cpu*) #x42)
    (cpu::write-memory 0 #xE0)  ; CPX immediate
    (cpu::write-memory 1 #x42)
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be set when values are equal")
    (ok (= 1 (cpu::get-flag :carry)) "Carry flag should be set when X >= M"))

  (testing "cpx - carry flag when x register greater than memory"
    (setup-test-cpu)
    (setf (cpu::cpu-x-register cpu::*default-cpu*) #x42)
    (cpu::write-memory 0 #xE0)  ; CPX immediate
    (cpu::write-memory 1 #x40)
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :carry)) "Carry flag should be set when X >= M")
    (ok (= 0 (cpu::get-flag :zero)) "Zero flag should be clear when values are not equal"))

  (testing "cpx - negative flag when result has bit 7 set"
    (setup-test-cpu)
    (setf (cpu::cpu-x-register cpu::*default-cpu*) #x40)
    (cpu::write-memory 0 #xE0)  ; CPX immediate
    (cpu::write-memory 1 #xC0)  ; Results in #x80 (negative)
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be set when result has bit 7 set")
    (ok (= 0 (cpu::get-flag :carry)) "Carry flag should be clear when X < M"))

  (testing "cpx - verify x register unchanged"
    (setup-test-cpu)
    (setf (cpu::cpu-x-register cpu::*default-cpu*) #x42)
    (cpu::write-memory 0 #xE0)  ; CPX immediate
    (cpu::write-memory 1 #x40)
    (cpu::cpu-step)
    (ok (= #x42 (cpu::cpu-x-register cpu::*default-cpu*)) "X register should remain unchanged")))

(deftest test-cpy
  (testing "cpy - zero flag when y register equals memory"
    (setup-test-cpu)
    (setf (cpu::cpu-y-register cpu::*default-cpu*) #x42)
    (cpu::write-memory 0 #xC0)  ; CPY immediate
    (cpu::write-memory 1 #x42)
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be set when values are equal")
    (ok (= 1 (cpu::get-flag :carry)) "Carry flag should be set when Y >= M"))

  (testing "cpy - carry flag when y register greater than memory"
    (setup-test-cpu)
    (setf (cpu::cpu-y-register cpu::*default-cpu*) #x42)
    (cpu::write-memory 0 #xC0)  ; CPY immediate
    (cpu::write-memory 1 #x40)
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :carry)) "Carry flag should be set when Y >= M")
    (ok (= 0 (cpu::get-flag :zero)) "Zero flag should be clear when values are not equal"))

  (testing "cpy - negative flag when result has bit 7 set"
    (setup-test-cpu)
    (setf (cpu::cpu-y-register cpu::*default-cpu*) #x40)
    (cpu::write-memory 0 #xC0)  ; CPY immediate
    (cpu::write-memory 1 #xC0)  ; Results in #x80 (negative)
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be set when result has bit 7 set")
    (ok (= 0 (cpu::get-flag :carry)) "Carry flag should be clear when Y < M"))

  (testing "cpy - verify y register unchanged"
    (setup-test-cpu)
    (setf (cpu::cpu-y-register cpu::*default-cpu*) #x42)
    (cpu::write-memory 0 #xC0)  ; CPY immediate
    (cpu::write-memory 1 #x40)
    (cpu::cpu-step)
    (ok (= #x42 (cpu::cpu-y-register cpu::*default-cpu*)) "Y register should remain unchanged")))

(deftest test-dec
  (testing "dec - basic decrement"
    (setup-test-cpu)
    (cpu::write-memory 0 #xC6)  ; DEC zero page
    (cpu::write-memory 1 #x50)  ; Zero page address
    (cpu::write-memory #x50 #x42)  ; Value to decrement
    (cpu::cpu-step)
    (ok (= #x41 (cpu::read-memory #x50)) "Memory value should decrease by one"))

  (testing "dec - zero flag when result is zero"
    (setup-test-cpu)
    (cpu::write-memory 0 #xC6)  ; DEC zero page
    (cpu::write-memory 1 #x50)
    (cpu::write-memory #x50 #x01)  ; Will decrement to zero
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be set when result is zero")
    (ok (= #x00 (cpu::read-memory #x50)) "Memory value should be zero"))

  (testing "dec - negative flag when bit 7 is set"
    (setup-test-cpu)
    (cpu::write-memory 0 #xC6)  ; DEC zero page
    (cpu::write-memory 1 #x50)
    (cpu::write-memory #x50 #x00)  ; Will decrement to #xFF
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be set when bit 7 is set")
    (ok (= #xFF (cpu::read-memory #x50)) "Memory value should wrap to #xFF"))

  (testing "dec - wrapping from 0 to FF"
    (setup-test-cpu)
    (cpu::write-memory 0 #xC6)  ; DEC zero page
    (cpu::write-memory 1 #x50)
    (cpu::write-memory #x50 #x00)
    (cpu::cpu-step)
    (ok (= #xFF (cpu::read-memory #x50)) "Memory value should wrap from #x00 to #xFF")))

(deftest test-dex
  (testing "dex - basic decrement"
    (setup-test-cpu)
    (setf (cpu::cpu-x-register cpu::*default-cpu*) #x42)
    (cpu::write-memory 0 #xCA)  ; DEX
    (cpu::cpu-step)
    (ok (= #x41 (cpu::cpu-x-register cpu::*default-cpu*)) "X register should decrease by one"))

  (testing "dex - zero flag when result is zero"
    (setup-test-cpu)
    (setf (cpu::cpu-x-register cpu::*default-cpu*) #x01)
    (cpu::write-memory 0 #xCA)  ; DEX
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be set when result is zero")
    (ok (= #x00 (cpu::cpu-x-register cpu::*default-cpu*)) "X register should be zero"))

  (testing "dex - negative flag when bit 7 is set"
    (setup-test-cpu)
    (setf (cpu::cpu-x-register cpu::*default-cpu*) #x00)
    (cpu::write-memory 0 #xCA)  ; DEX
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be set when bit 7 is set")
    (ok (= #xFF (cpu::cpu-x-register cpu::*default-cpu*)) "X register should wrap to #xFF"))

  (testing "dex - wrapping from 0 to FF"
    (setup-test-cpu)
    (setf (cpu::cpu-x-register cpu::*default-cpu*) #x00)
    (cpu::write-memory 0 #xCA)  ; DEX
    (cpu::cpu-step)
    (ok (= #xFF (cpu::cpu-x-register cpu::*default-cpu*)) "X register should wrap from #x00 to #xFF")))

(deftest test-dey
  (testing "dey - basic decrement"
    (setup-test-cpu)
    (setf (cpu::cpu-y-register cpu::*default-cpu*) #x42)
    (cpu::write-memory 0 #x88)  ; DEY
    (cpu::cpu-step)
    (ok (= #x41 (cpu::cpu-y-register cpu::*default-cpu*)) "Y register should decrease by one"))

  (testing "dey - zero flag when result is zero"
    (setup-test-cpu)
    (setf (cpu::cpu-y-register cpu::*default-cpu*) #x01)
    (cpu::write-memory 0 #x88)  ; DEY
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be set when result is zero")
    (ok (= #x00 (cpu::cpu-y-register cpu::*default-cpu*)) "Y register should be zero"))

  (testing "dey - negative flag when bit 7 is set"
    (setup-test-cpu)
    (setf (cpu::cpu-y-register cpu::*default-cpu*) #x00)
    (cpu::write-memory 0 #x88)  ; DEY
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be set when bit 7 is set")
    (ok (= #xFF (cpu::cpu-y-register cpu::*default-cpu*)) "Y register should wrap to #xFF"))

  (testing "dey - wrapping from 0 to FF"
    (setup-test-cpu)
    (setf (cpu::cpu-y-register cpu::*default-cpu*) #x00)
    (cpu::write-memory 0 #x88)  ; DEY
    (cpu::cpu-step)
    (ok (= #xFF (cpu::cpu-y-register cpu::*default-cpu*)) "Y register should wrap from #x00 to #xFF")))

(deftest test-eor
  (testing "eor - basic exclusive or operation"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #b10101010)
    (cpu::write-memory 0 #x49)  ; EOR immediate
    (cpu::write-memory 1 #b11110000)
    (cpu::cpu-step)
    (ok (= #b01011010 (cpu::cpu-accumulator cpu::*default-cpu*)) "Accumulator should contain A XOR #F0"))

  (testing "eor - zero flag when result is zero"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #b11110000)
    (cpu::write-memory 0 #x49)  ; EOR immediate
    (cpu::write-memory 1 #b11110000)  ; Same value results in zero
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be set when result is zero")
    (ok (= #x00 (cpu::cpu-accumulator cpu::*default-cpu*)) "Accumulator should be zero"))

  (testing "eor - negative flag when bit 7 is set"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #b00110011)
    (cpu::write-memory 0 #x49)  ; EOR immediate
    (cpu::write-memory 1 #b10101010)  ; Results in negative number
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be set when bit 7 is set")
    (ok (= #b10011001 (cpu::cpu-accumulator cpu::*default-cpu*)) "Accumulator should have bit 7 set")))

(deftest test-inc
  (testing "inc - basic increment"
    (setup-test-cpu)
    (cpu::write-memory 0 #xE6)  ; INC zero page
    (cpu::write-memory 1 #x50)  ; Zero page address
    (cpu::write-memory #x50 #x42)  ; Value to increment
    (cpu::cpu-step)
    (ok (= #x43 (cpu::read-memory #x50)) "Memory value should increase by one"))

  (testing "inc - zero flag when result is zero"
    (setup-test-cpu)
    (cpu::write-memory 0 #xE6)  ; INC zero page
    (cpu::write-memory 1 #x50)
    (cpu::write-memory #x50 #xFF)  ; Will increment to zero
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be set when result is zero")
    (ok (= #x00 (cpu::read-memory #x50)) "Memory value should be zero"))

  (testing "inc - negative flag when bit 7 is set"
    (setup-test-cpu)
    (cpu::write-memory 0 #xE6)  ; INC zero page
    (cpu::write-memory 1 #x50)
    (cpu::write-memory #x50 #x7F)  ; Will increment to #x80
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be set when bit 7 is set")
    (ok (= #x80 (cpu::read-memory #x50)) "Memory value should be #x80"))

  (testing "inc - wrapping from FF to 00"
    (setup-test-cpu)
    (cpu::write-memory 0 #xE6)  ; INC zero page
    (cpu::write-memory 1 #x50)
    (cpu::write-memory #x50 #xFF)
    (cpu::cpu-step)
    (ok (= #x00 (cpu::read-memory #x50)) "Memory value should wrap from #xFF to #x00")))

(deftest test-inx
  (testing "inx - basic increment"
    (setup-test-cpu)
    (setf (cpu::cpu-x-register cpu::*default-cpu*) #x42)
    (cpu::write-memory 0 #xE8)  ; INX
    (cpu::cpu-step)
    (ok (= #x43 (cpu::cpu-x-register cpu::*default-cpu*)) "X register should increase by one"))

  (testing "inx - zero flag when result is zero"
    (setup-test-cpu)
    (setf (cpu::cpu-x-register cpu::*default-cpu*) #xFF)
    (cpu::write-memory 0 #xE8)  ; INX
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be set when result is zero")
    (ok (= #x00 (cpu::cpu-x-register cpu::*default-cpu*)) "X register should be zero"))

  (testing "inx - negative flag when bit 7 is set"
    (setup-test-cpu)
    (setf (cpu::cpu-x-register cpu::*default-cpu*) #x7F)
    (cpu::write-memory 0 #xE8)  ; INX
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be set when bit 7 is set")
    (ok (= #x80 (cpu::cpu-x-register cpu::*default-cpu*)) "X register should be #x80"))

  (testing "inx - wrapping from FF to 00"
    (setup-test-cpu)
    (setf (cpu::cpu-x-register cpu::*default-cpu*) #xFF)
    (cpu::write-memory 0 #xE8)  ; INX
    (cpu::cpu-step)
    (ok (= #x00 (cpu::cpu-x-register cpu::*default-cpu*)) "X register should wrap from #xFF to #x00")))

(deftest test-iny
  (testing "iny - basic increment"
    (setup-test-cpu)
    (setf (cpu::cpu-y-register cpu::*default-cpu*) #x42)
    (cpu::write-memory 0 #xC8)  ; INY
    (cpu::cpu-step)
    (ok (= #x43 (cpu::cpu-y-register cpu::*default-cpu*)) "Y register should increase by one"))

  (testing "iny - zero flag when result is zero"
    (setup-test-cpu)
    (setf (cpu::cpu-y-register cpu::*default-cpu*) #xFF)
    (cpu::write-memory 0 #xC8)  ; INY
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be set when result is zero")
    (ok (= #x00 (cpu::cpu-y-register cpu::*default-cpu*)) "Y register should be zero"))

  (testing "iny - negative flag when bit 7 is set"
    (setup-test-cpu)
    (setf (cpu::cpu-y-register cpu::*default-cpu*) #x7F)
    (cpu::write-memory 0 #xC8)  ; INY
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be set when bit 7 is set")
    (ok (= #x80 (cpu::cpu-y-register cpu::*default-cpu*)) "Y register should be #x80"))

  (testing "iny - wrapping from FF to 00"
    (setup-test-cpu)
    (setf (cpu::cpu-y-register cpu::*default-cpu*) #xFF)
    (cpu::write-memory 0 #xC8)  ; INY
    (cpu::cpu-step)
    (ok (= #x00 (cpu::cpu-y-register cpu::*default-cpu*)) "Y register should wrap from #xFF to #x00")))

(deftest test-jmp
  (testing "jmp - absolute addressing"
    (setup-test-cpu)
    (cpu::write-memory 0 #x4C)  ; JMP absolute
    (cpu::write-memory 1 #x34)  ; Low byte of target address
    (cpu::write-memory 2 #x12)  ; High byte of target address
    (cpu::cpu-step)
    (ok (= #x1234 (cpu::cpu-program-counter cpu::*default-cpu*)) "Program counter should jump to #x1234"))

  (testing "jmp - indirect addressing"
    (setup-test-cpu)
    (cpu::write-memory 0 #x6C)  ; JMP indirect
    (cpu::write-memory 1 #x20)  ; Low byte of pointer
    (cpu::write-memory 2 #x00)  ; High byte of pointer
    (cpu::write-memory #x20 #x34)  ; Low byte of target address
    (cpu::write-memory #x21 #x12)  ; High byte of target address
    (cpu::cpu-step)
    (ok (= #x1234 (cpu::cpu-program-counter cpu::*default-cpu*)) "Program counter should jump to address stored at #x0020"))

  (testing "jmp - indirect page boundary bug"
    (setup-test-cpu)
    (cpu::write-memory 0 #x6C)     ; JMP indirect
    (cpu::write-memory 1 #xFF)     ; Low byte of pointer
    (cpu::write-memory 2 #x02)     ; High byte of pointer
    (cpu::write-memory #x02FF #x34) ; Low byte of target
    (cpu::write-memory #x0200 #x12) ; High byte should be read from here (not #x0300)
    (cpu::cpu-step)
    (ok (= #x1234 (cpu::cpu-program-counter cpu::*default-cpu*))
        "Program counter should use wrapped address for high byte")))

(deftest test-jsr
  (testing "jsr - program counter update"
    (setup-test-cpu)
    (cpu::write-memory 0 #x20)  ; JSR absolute
    (cpu::write-memory 1 #x34)  ; Low byte of target address
    (cpu::write-memory 2 #x12)  ; High byte of target address
    (cpu::cpu-step)
    (ok (= #x1234 (cpu::cpu-program-counter cpu::*default-cpu*)) "Program counter should jump to subroutine address"))

  (testing "jsr - return address pushed to stack"
    (setup-test-cpu)
    (setf (cpu::cpu-stack-pointer cpu::*default-cpu*) #xFF)  ; Initialize stack pointer
    (cpu::write-memory 0 #x20)  ; JSR absolute
    (cpu::write-memory 1 #x34)  ; Low byte of target
    (cpu::write-memory 2 #x12)  ; High byte of target
    (cpu::cpu-step)
    ;; Return address should be 2 (pointing to the byte after JSR)
    (ok (= #x02 (cpu::read-memory #x01FF)) "High byte of return address should be pushed to stack")
    (ok (= #x00 (cpu::read-memory #x01FE)) "Low byte of return address should be pushed to stack"))

  (testing "jsr - stack pointer decremented"
    (setup-test-cpu)
    (setf (cpu::cpu-stack-pointer cpu::*default-cpu*) #xFF)
    (cpu::write-memory 0 #x20)  ; JSR absolute
    (cpu::write-memory 1 #x34)
    (cpu::write-memory 2 #x12)
    (cpu::cpu-step)
    (ok (= #xFD (cpu::cpu-stack-pointer cpu::*default-cpu*)) "Stack pointer should decrease by 2")))

(deftest test-lda
  (testing "lda - basic load"
    (setup-test-cpu)
    (cpu::write-memory 0 #xA9)  ; LDA immediate
    (cpu::write-memory 1 #x42)  ; Value to load
    (cpu::cpu-step)
    (ok (= #x42 (cpu::cpu-accumulator cpu::*default-cpu*)) "Accumulator should contain loaded value"))

  (testing "lda - zero flag when loading zero"
    (setup-test-cpu)
    (cpu::write-memory 0 #xA9)  ; LDA immediate
    (cpu::write-memory 1 #x00)
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be set when loading zero")
    (ok (= #x00 (cpu::cpu-accumulator cpu::*default-cpu*)) "Accumulator should be zero"))

  (testing "lda - negative flag when bit 7 is set"
    (setup-test-cpu)
    (cpu::write-memory 0 #xA9)  ; LDA immediate
    (cpu::write-memory 1 #x80)  ; Value with bit 7 set
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be set when bit 7 is set")
    (ok (= #x80 (cpu::cpu-accumulator cpu::*default-cpu*)) "Accumulator should have high bit set"))

  (testing "lda - flags cleared when loading positive non-zero value"
    (setup-test-cpu)
    (cpu::set-flag :zero 1)
    (cpu::set-flag :negative 1)
    (cpu::write-memory 0 #xA9)  ; LDA immediate
    (cpu::write-memory 1 #x42)
    (cpu::cpu-step)
    (ok (= 0 (cpu::get-flag :zero)) "Zero flag should be cleared")
    (ok (= 0 (cpu::get-flag :negative)) "Negative flag should be cleared")))

(deftest test-ldx
  (testing "ldx - basic load"
    (setup-test-cpu)
    (cpu::write-memory 0 #xA2)  ; LDX immediate
    (cpu::write-memory 1 #x42)  ; Value to load
    (cpu::cpu-step)
    (ok (= #x42 (cpu::cpu-x-register cpu::*default-cpu*)) "X register should contain loaded value"))

  (testing "ldx - zero flag when loading zero"
    (setup-test-cpu)
    (cpu::write-memory 0 #xA2)  ; LDX immediate
    (cpu::write-memory 1 #x00)
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be set when loading zero")
    (ok (= #x00 (cpu::cpu-x-register cpu::*default-cpu*)) "X register should be zero"))

  (testing "ldx - negative flag when bit 7 is set"
    (setup-test-cpu)
    (cpu::write-memory 0 #xA2)  ; LDX immediate
    (cpu::write-memory 1 #x80)  ; Value with bit 7 set
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be set when bit 7 is set")
    (ok (= #x80 (cpu::cpu-x-register cpu::*default-cpu*)) "X register should have high bit set"))

  (testing "ldx - flags cleared when loading positive non-zero value"
    (setup-test-cpu)
    (cpu::set-flag :zero 1)
    (cpu::set-flag :negative 1)
    (cpu::write-memory 0 #xA2)  ; LDX immediate
    (cpu::write-memory 1 #x42)
    (cpu::cpu-step)
    (ok (= 0 (cpu::get-flag :zero)) "Zero flag should be cleared")
    (ok (= 0 (cpu::get-flag :negative)) "Negative flag should be cleared")))

(deftest test-ldy
  (testing "ldy - basic load"
    (setup-test-cpu)
    (cpu::write-memory 0 #xA0)  ; LDY immediate
    (cpu::write-memory 1 #x42)  ; Value to load
    (cpu::cpu-step)
    (ok (= #x42 (cpu::cpu-y-register cpu::*default-cpu*)) "Y register should contain loaded value"))

  (testing "ldy - zero flag when loading zero"
    (setup-test-cpu)
    (cpu::write-memory 0 #xA0)  ; LDY immediate
    (cpu::write-memory 1 #x00)
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be set when loading zero")
    (ok (= #x00 (cpu::cpu-y-register cpu::*default-cpu*)) "Y register should be zero"))

  (testing "ldy - negative flag when bit 7 is set"
    (setup-test-cpu)
    (cpu::write-memory 0 #xA0)  ; LDY immediate
    (cpu::write-memory 1 #x80)  ; Value with bit 7 set
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be set when bit 7 is set")
    (ok (= #x80 (cpu::cpu-y-register cpu::*default-cpu*)) "Y register should have high bit set"))

  (testing "ldy - flags cleared when loading positive non-zero value"
    (setup-test-cpu)
    (cpu::set-flag :zero 1)
    (cpu::set-flag :negative 1)
    (cpu::write-memory 0 #xA0)  ; LDY immediate
    (cpu::write-memory 1 #x42)
    (cpu::cpu-step)
    (ok (= 0 (cpu::get-flag :zero)) "Zero flag should be cleared")
    (ok (= 0 (cpu::get-flag :negative)) "Negative flag should be cleared")))

(deftest test-lsr
  (testing "lsr - basic shift accumulator"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #b10101010)
    (cpu::write-memory 0 #x4A)  ; LSR accumulator
    (cpu::cpu-step)
    (ok (= #b01010101 (cpu::cpu-accumulator cpu::*default-cpu*)) "Accumulator should be shifted right one position"))

  (testing "lsr - carry flag gets old bit 0"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #b10101011)
    (cpu::write-memory 0 #x4A)  ; LSR accumulator
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :carry)) "Carry flag should contain the old bit 0")
    (ok (= #b01010101 (cpu::cpu-accumulator cpu::*default-cpu*)) "Accumulator should be shifted right"))

  (testing "lsr - zero flag when result is zero"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #b00000001)
    (cpu::write-memory 0 #x4A)  ; LSR accumulator
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be set when result is 0")
    (ok (= #b00000000 (cpu::cpu-accumulator cpu::*default-cpu*)) "Accumulator should be zero"))

  (testing "lsr - negative flag is always clear"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #b11111111)
    (cpu::write-memory 0 #x4A)  ; LSR accumulator
    (cpu::cpu-step)
    (ok (= 0 (cpu::get-flag :negative)) "Negative flag should always be clear")
    (ok (= #b01111111 (cpu::cpu-accumulator cpu::*default-cpu*)) "Bit 7 should be 0")))

(deftest test-nop
  (testing "nop - advances program counter"
    (setup-test-cpu)
    (cpu::write-memory 0 #xEA)  ; NOP
    (let ((old-pc (cpu::cpu-program-counter cpu::*default-cpu*)))
      (cpu::cpu-step)
      (ok (= (+ old-pc 1) (cpu::cpu-program-counter cpu::*default-cpu*))
          "Program counter should advance by one")))

  (testing "nop - preserves all registers"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #x42)
    (setf (cpu::cpu-x-register cpu::*default-cpu*) #x43)
    (setf (cpu::cpu-y-register cpu::*default-cpu*) #x44)
    (cpu::write-memory 0 #xEA)  ; NOP
    (cpu::cpu-step)
    (ok (= #x42 (cpu::cpu-accumulator cpu::*default-cpu*)) "Accumulator should be unchanged")
    (ok (= #x43 (cpu::cpu-x-register cpu::*default-cpu*)) "X register should be unchanged")
    (ok (= #x44 (cpu::cpu-y-register cpu::*default-cpu*)) "Y register should be unchanged"))

  (testing "nop - preserves all flags"
    (setup-test-cpu)
    (cpu::set-flag :zero 1)
    (cpu::set-flag :negative 1)
    (cpu::set-flag :carry 1)
    (cpu::set-flag :overflow 1)
    (cpu::set-flag :decimal 1)
    (cpu::set-flag :interrupt 1)
    (cpu::write-memory 0 #xEA)  ; NOP
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be unchanged")
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be unchanged")
    (ok (= 1 (cpu::get-flag :carry)) "Carry flag should be unchanged")
    (ok (= 1 (cpu::get-flag :overflow)) "Overflow flag should be unchanged")
    (ok (= 1 (cpu::get-flag :decimal)) "Decimal flag should be unchanged")
    (ok (= 1 (cpu::get-flag :interrupt)) "Interrupt flag should be unchanged")))

(deftest test-pha
  (fail "PHA Not Implemented."))

(deftest test-php
  (fail "PHP Not Implemented."))

(deftest test-pla
  (fail "PLA Not Implemented."))

(deftest test-plp
  (fail "PLP Not Implemented."))

(deftest test-rol
  (testing "rol - basic rotate left accumulator"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #b10101010)
    (cpu::set-flag :carry 0)
    (cpu::write-memory 0 #x2A)  ; ROL accumulator
    (cpu::cpu-step)
    (ok (= #b01010100 (cpu::cpu-accumulator cpu::*default-cpu*)) "Result should be shifted left with carry 0 input")
    (ok (= 1 (cpu::get-flag :carry)) "Carry flag should contain old bit 7"))

  (testing "rol - rotate with carry set"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #b10101010)
    (cpu::set-flag :carry 1)
    (cpu::write-memory 0 #x2A)  ; ROL accumulator
    (cpu::cpu-step)
    (ok (= #b01010101 (cpu::cpu-accumulator cpu::*default-cpu*)) "Result should be shifted left with carry 1 input")
    (ok (= 1 (cpu::get-flag :carry)) "Carry flag should contain old bit 7"))

  (testing "rol - zero flag when result is zero"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #b10000000)
    (cpu::set-flag :carry 0)
    (cpu::write-memory 0 #x2A)  ; ROL accumulator
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be set when result is 0")
    (ok (= #b00000000 (cpu::cpu-accumulator cpu::*default-cpu*)) "Result should be zero"))

  (testing "rol - negative flag when result has bit 7 set"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #b01000000)
    (cpu::set-flag :carry 0)
    (cpu::write-memory 0 #x2A)  ; ROL accumulator
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be set when result has bit 7 set")
    (ok (= #b10000000 (cpu::cpu-accumulator cpu::*default-cpu*)) "Result should have bit 7 set")))

(deftest test-ror
  (testing "ror - basic rotate right accumulator"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #b10101010)
    (cpu::set-flag :carry 0)
    (cpu::write-memory 0 #x6A)  ; ROR accumulator
    (cpu::cpu-step)
    (ok (= #b01010101 (cpu::cpu-accumulator cpu::*default-cpu*)) "Result should be shifted right with carry 0 input")
    (ok (= 0 (cpu::get-flag :carry)) "Carry flag should contain old bit 0"))

  (testing "ror - rotate with carry set"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #b10101010)
    (cpu::set-flag :carry 1)
    (cpu::write-memory 0 #x6A)  ; ROR accumulator
    (cpu::cpu-step)
    (ok (= #b11010101 (cpu::cpu-accumulator cpu::*default-cpu*)) "Result should be shifted right with carry 1 input")
    (ok (= 0 (cpu::get-flag :carry)) "Carry flag should contain old bit 0"))

  (testing "ror - zero flag when result is zero"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #b00000001)
    (cpu::set-flag :carry 0)
    (cpu::write-memory 0 #x6A)  ; ROR accumulator
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be set when result is 0")
    (ok (= #b00000000 (cpu::cpu-accumulator cpu::*default-cpu*)) "Result should be zero"))

  (testing "ror - negative flag when result has bit 7 set"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #b00000001)
    (cpu::set-flag :carry 1)
    (cpu::write-memory 0 #x6A)  ; ROR accumulator
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be set when result has bit 7 set")
    (ok (= #b10000000 (cpu::cpu-accumulator cpu::*default-cpu*)) "Result should have bit 7 set")))

(deftest test-rti
  (fail "RTI Not Implemented."))

(deftest test-rts
  (fail "RTS Not Implemented."))

(deftest test-sbc
  (testing "sbc - basic subtraction"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #x50)
    (cpu::set-flag :carry 1)  ; Set carry (no borrow)
    (cpu::write-memory 0 #xE9)  ; SBC immediate
    (cpu::write-memory 1 #x20)
    (cpu::cpu-step)
    (ok (= #x30 (cpu::cpu-accumulator cpu::*default-cpu*)) "Accumulator should contain #x50 - #x20"))

  (testing "sbc - with borrow (carry clear)"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #x50)
    (cpu::set-flag :carry 0)  ; Clear carry (borrow)
    (cpu::write-memory 0 #xE9)  ; SBC immediate
    (cpu::write-memory 1 #x20)
    (cpu::cpu-step)
    (ok (= #x2F (cpu::cpu-accumulator cpu::*default-cpu*)) "Accumulator should contain #x50 - #x20 - 1"))

  (testing "sbc - carry flag set when no borrow needed"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #x50)
    (cpu::set-flag :carry 1)
    (cpu::write-memory 0 #xE9)  ; SBC immediate
    (cpu::write-memory 1 #x20)
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :carry)) "Carry flag should be set when no borrow needed"))

  (testing "sbc - carry flag clear when borrow needed"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #x20)
    (cpu::set-flag :carry 1)
    (cpu::write-memory 0 #xE9)  ; SBC immediate
    (cpu::write-memory 1 #x30)
    (cpu::cpu-step)
    (ok (= 0 (cpu::get-flag :carry)) "Carry flag should be clear when borrow needed"))

  (testing "sbc - zero flag when result is zero"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #x20)
    (cpu::set-flag :carry 1)
    (cpu::write-memory 0 #xE9)  ; SBC immediate
    (cpu::write-memory 1 #x20)
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be set when result is zero"))

  (testing "sbc - negative flag when result has bit 7 set"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #x20)
    (cpu::set-flag :carry 1)
    (cpu::write-memory 0 #xE9)  ; SBC immediate
    (cpu::write-memory 1 #x30)
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be set when result is negative"))

  (testing "sbc - overflow flag on signed overflow"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #x50)  ; +80
    (cpu::set-flag :carry 1)
    (cpu::write-memory 0 #xE9)  ; SBC immediate
    (cpu::write-memory 1 #xB0)  ; -80 (subtracting negative causes overflow)
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :overflow)) "Overflow flag should be set on signed overflow")))

(deftest test-sec
  (testing "sec - set carry flag when clear"
    (setup-test-cpu)
    (cpu::set-flag :carry 0)  ; Clear carry flag
    (cpu::write-memory 0 #x38)  ; SEC
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :carry)) "Carry flag should be set"))

  (testing "sec - set carry flag when already set"
    (setup-test-cpu)
    (cpu::set-flag :carry 1)  ; Set carry flag
    (cpu::write-memory 0 #x38)  ; SEC
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :carry)) "Carry flag should remain set"))

  (testing "sec - doesn't affect other flags"
    (setup-test-cpu)
    (cpu::set-flag :zero 1)
    (cpu::set-flag :overflow 1)
    (cpu::set-flag :negative 1)
    (cpu::set-flag :decimal 1)
    (cpu::set-flag :interrupt 1)
    (cpu::write-memory 0 #x38)  ; SEC
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be unchanged")
    (ok (= 1 (cpu::get-flag :overflow)) "Overflow flag should be unchanged")
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be unchanged")
    (ok (= 1 (cpu::get-flag :decimal)) "Decimal flag should be unchanged")
    (ok (= 1 (cpu::get-flag :interrupt)) "Interrupt flag should be unchanged")))

(deftest test-sed
  (testing "sed - set decimal flag when clear"
    (setup-test-cpu)
    (cpu::set-flag :decimal 0)  ; Clear decimal flag
    (cpu::write-memory 0 #xF8)  ; SED
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :decimal)) "Decimal flag should be set"))

  (testing "sed - set decimal flag when already set"
    (setup-test-cpu)
    (cpu::set-flag :decimal 1)  ; Set decimal flag
    (cpu::write-memory 0 #xF8)  ; SED
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :decimal)) "Decimal flag should remain set"))

  (testing "sed - doesn't affect other flags"
    (setup-test-cpu)
    (cpu::set-flag :zero 1)
    (cpu::set-flag :overflow 1)
    (cpu::set-flag :negative 1)
    (cpu::set-flag :carry 1)
    (cpu::set-flag :interrupt 1)
    (cpu::write-memory 0 #xF8)  ; SED
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be unchanged")
    (ok (= 1 (cpu::get-flag :overflow)) "Overflow flag should be unchanged")
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be unchanged")
    (ok (= 1 (cpu::get-flag :carry)) "Carry flag should be unchanged")
    (ok (= 1 (cpu::get-flag :interrupt)) "Interrupt flag should be unchanged")))

(deftest test-sei
  (testing "sei - set interrupt flag when clear"
    (setup-test-cpu)
    (cpu::set-flag :interrupt 0)  ; Clear interrupt flag
    (cpu::write-memory 0 #x78)  ; SEI
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :interrupt)) "Interrupt flag should be set"))

  (testing "sei - set interrupt flag when already set"
    (setup-test-cpu)
    (cpu::set-flag :interrupt 1)  ; Set interrupt flag
    (cpu::write-memory 0 #x78)  ; SEI
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :interrupt)) "Interrupt flag should remain set"))

  (testing "sei - doesn't affect other flags"
    (setup-test-cpu)
    (cpu::set-flag :zero 1)
    (cpu::set-flag :overflow 1)
    (cpu::set-flag :negative 1)
    (cpu::set-flag :carry 1)
    (cpu::set-flag :decimal 1)
    (cpu::write-memory 0 #x78)  ; SEI
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be unchanged")
    (ok (= 1 (cpu::get-flag :overflow)) "Overflow flag should be unchanged")
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be unchanged")
    (ok (= 1 (cpu::get-flag :carry)) "Carry flag should be unchanged")
    (ok (= 1 (cpu::get-flag :decimal)) "Decimal flag should be unchanged")))

(deftest test-sta
  (testing "sta - basic store"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #x42)
    (cpu::write-memory 0 #x85)  ; STA zero page
    (cpu::write-memory 1 #x50)  ; Zero page address
    (cpu::cpu-step)
    (ok (= #x42 (cpu::read-memory #x50)) "Memory should contain accumulator value"))

  (testing "sta - preserves accumulator value"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #x42)
    (cpu::write-memory 0 #x85)  ; STA zero page
    (cpu::write-memory 1 #x50)
    (cpu::cpu-step)
    (ok (= #x42 (cpu::cpu-accumulator cpu::*default-cpu*)) "Accumulator should be unchanged"))

  (testing "sta - doesn't affect any flags"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #x80)  ; Negative value
    (cpu::set-flag :zero 1)
    (cpu::set-flag :negative 0)
    (cpu::set-flag :carry 1)
    (cpu::set-flag :overflow 1)
    (cpu::write-memory 0 #x85)  ; STA zero page
    (cpu::write-memory 1 #x50)
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be unchanged")
    (ok (= 0 (cpu::get-flag :negative)) "Negative flag should be unchanged")
    (ok (= 1 (cpu::get-flag :carry)) "Carry flag should be unchanged")
    (ok (= 1 (cpu::get-flag :overflow)) "Overflow flag should be unchanged")))

(deftest test-stx
  (testing "stx - basic store"
    (setup-test-cpu)
    (setf (cpu::cpu-x-register cpu::*default-cpu*) #x42)
    (cpu::write-memory 0 #x86)  ; STX zero page
    (cpu::write-memory 1 #x50)  ; Zero page address
    (cpu::cpu-step)
    (ok (= #x42 (cpu::read-memory #x50)) "Memory should contain X register value"))

  (testing "stx - preserves x register value"
    (setup-test-cpu)
    (setf (cpu::cpu-x-register cpu::*default-cpu*) #x42)
    (cpu::write-memory 0 #x86)  ; STX zero page
    (cpu::write-memory 1 #x50)
    (cpu::cpu-step)
    (ok (= #x42 (cpu::cpu-x-register cpu::*default-cpu*)) "X register should be unchanged"))

  (testing "stx - doesn't affect any flags"
    (setup-test-cpu)
    (setf (cpu::cpu-x-register cpu::*default-cpu*) #x80)  ; Negative value
    (cpu::set-flag :zero 1)
    (cpu::set-flag :negative 0)
    (cpu::set-flag :carry 1)
    (cpu::set-flag :overflow 1)
    (cpu::write-memory 0 #x86)  ; STX zero page
    (cpu::write-memory 1 #x50)
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be unchanged")
    (ok (= 0 (cpu::get-flag :negative)) "Negative flag should be unchanged")
    (ok (= 1 (cpu::get-flag :carry)) "Carry flag should be unchanged")
    (ok (= 1 (cpu::get-flag :overflow)) "Overflow flag should be unchanged")))

(deftest test-sty
  (testing "sty - basic store"
    (setup-test-cpu)
    (setf (cpu::cpu-y-register cpu::*default-cpu*) #x42)
    (cpu::write-memory 0 #x84)  ; STY zero page
    (cpu::write-memory 1 #x50)  ; Zero page address
    (cpu::cpu-step)
    (ok (= #x42 (cpu::read-memory #x50)) "Memory should contain Y register value"))

  (testing "sty - preserves y register value"
    (setup-test-cpu)
    (setf (cpu::cpu-y-register cpu::*default-cpu*) #x42)
    (cpu::write-memory 0 #x84)  ; STY zero page
    (cpu::write-memory 1 #x50)
    (cpu::cpu-step)
    (ok (= #x42 (cpu::cpu-y-register cpu::*default-cpu*)) "Y register should be unchanged"))

  (testing "sty - doesn't affect any flags"
    (setup-test-cpu)
    (setf (cpu::cpu-y-register cpu::*default-cpu*) #x80)  ; Negative value
    (cpu::set-flag :zero 1)
    (cpu::set-flag :negative 0)
    (cpu::set-flag :carry 1)
    (cpu::set-flag :overflow 1)
    (cpu::write-memory 0 #x84)  ; STY zero page
    (cpu::write-memory 1 #x50)
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be unchanged")
    (ok (= 0 (cpu::get-flag :negative)) "Negative flag should be unchanged")
    (ok (= 1 (cpu::get-flag :carry)) "Carry flag should be unchanged")
    (ok (= 1 (cpu::get-flag :overflow)) "Overflow flag should be unchanged")))

(deftest test-tax
  (testing "tax - basic transfer"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #x42)
    (cpu::write-memory 0 #xAA)  ; TAX
    (cpu::cpu-step)
    (ok (= #x42 (cpu::cpu-x-register cpu::*default-cpu*)) "X register should contain accumulator value"))

  (testing "tax - preserves accumulator"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #x42)
    (cpu::write-memory 0 #xAA)  ; TAX
    (cpu::cpu-step)
    (ok (= #x42 (cpu::cpu-accumulator cpu::*default-cpu*)) "Accumulator should be unchanged"))

  (testing "tax - zero flag when transferring zero"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #x00)
    (cpu::write-memory 0 #xAA)  ; TAX
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be set when transferring zero")
    (ok (= #x00 (cpu::cpu-x-register cpu::*default-cpu*)) "X register should be zero"))

  (testing "tax - negative flag when transferring negative value"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #x80)
    (cpu::write-memory 0 #xAA)  ; TAX
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be set when transferring negative value")
    (ok (= #x80 (cpu::cpu-x-register cpu::*default-cpu*)) "X register should have high bit set")))

(deftest test-tay
  (testing "tay - basic transfer"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #x42)
    (cpu::write-memory 0 #xA8)  ; TAY
    (cpu::cpu-step)
    (ok (= #x42 (cpu::cpu-y-register cpu::*default-cpu*)) "Y register should contain accumulator value"))

  (testing "tay - preserves accumulator"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #x42)
    (cpu::write-memory 0 #xA8)  ; TAY
    (cpu::cpu-step)
    (ok (= #x42 (cpu::cpu-accumulator cpu::*default-cpu*)) "Accumulator should be unchanged"))

  (testing "tay - zero flag when transferring zero"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #x00)
    (cpu::write-memory 0 #xA8)  ; TAY
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be set when transferring zero")
    (ok (= #x00 (cpu::cpu-y-register cpu::*default-cpu*)) "Y register should be zero"))

  (testing "tay - negative flag when transferring negative value"
    (setup-test-cpu)
    (setf (cpu::cpu-accumulator cpu::*default-cpu*) #x80)
    (cpu::write-memory 0 #xA8)  ; TAY
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be set when transferring negative value")
    (ok (= #x80 (cpu::cpu-y-register cpu::*default-cpu*)) "Y register should have high bit set")))

(deftest test-tsx
  (testing "tsx - basic transfer"
    (setup-test-cpu)
    (setf (cpu::cpu-stack-pointer cpu::*default-cpu*) #x42)
    (cpu::write-memory 0 #xBA)  ; TSX
    (cpu::cpu-step)
    (ok (= #x42 (cpu::cpu-x-register cpu::*default-cpu*)) "X register should contain stack pointer value"))

  (testing "tsx - preserves stack pointer"
    (setup-test-cpu)
    (setf (cpu::cpu-stack-pointer cpu::*default-cpu*) #x42)
    (cpu::write-memory 0 #xBA)  ; TSX
    (cpu::cpu-step)
    (ok (= #x42 (cpu::cpu-stack-pointer cpu::*default-cpu*)) "Stack pointer should be unchanged"))

  (testing "tsx - zero flag when transferring zero"
    (setup-test-cpu)
    (setf (cpu::cpu-stack-pointer cpu::*default-cpu*) #x00)
    (cpu::write-memory 0 #xBA)  ; TSX
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be set when transferring zero")
    (ok (= #x00 (cpu::cpu-x-register cpu::*default-cpu*)) "X register should be zero"))

  (testing "tsx - negative flag when transferring negative value"
    (setup-test-cpu)
    (setf (cpu::cpu-stack-pointer cpu::*default-cpu*) #x80)
    (cpu::write-memory 0 #xBA)  ; TSX
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be set when transferring negative value")
    (ok (= #x80 (cpu::cpu-x-register cpu::*default-cpu*)) "X register should have high bit set")))

(deftest test-txa
  (testing "txa - basic transfer"
    (setup-test-cpu)
    (setf (cpu::cpu-x-register cpu::*default-cpu*) #x42)
    (cpu::write-memory 0 #x8A)  ; TXA
    (cpu::cpu-step)
    (ok (= #x42 (cpu::cpu-accumulator cpu::*default-cpu*)) "Accumulator should contain X register value"))

  (testing "txa - preserves x register"
    (setup-test-cpu)
    (setf (cpu::cpu-x-register cpu::*default-cpu*) #x42)
    (cpu::write-memory 0 #x8A)  ; TXA
    (cpu::cpu-step)
    (ok (= #x42 (cpu::cpu-x-register cpu::*default-cpu*)) "X register should be unchanged"))

  (testing "txa - zero flag when transferring zero"
    (setup-test-cpu)
    (setf (cpu::cpu-x-register cpu::*default-cpu*) #x00)
    (cpu::write-memory 0 #x8A)  ; TXA
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be set when transferring zero")
    (ok (= #x00 (cpu::cpu-accumulator cpu::*default-cpu*)) "Accumulator should be zero"))

  (testing "txa - negative flag when transferring negative value"
    (setup-test-cpu)
    (setf (cpu::cpu-x-register cpu::*default-cpu*) #x80)
    (cpu::write-memory 0 #x8A)  ; TXA
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be set when transferring negative value")
    (ok (= #x80 (cpu::cpu-accumulator cpu::*default-cpu*)) "Accumulator should have high bit set")))

(deftest test-txs
  (testing "txs - basic transfer"
    (setup-test-cpu)
    (setf (cpu::cpu-x-register cpu::*default-cpu*) #x42)
    (cpu::write-memory 0 #x9A)  ; TXS
    (cpu::cpu-step)
    (ok (= #x42 (cpu::cpu-stack-pointer cpu::*default-cpu*)) "Stack pointer should contain X register value"))

  (testing "txs - preserves x register"
    (setup-test-cpu)
    (setf (cpu::cpu-x-register cpu::*default-cpu*) #x42)
    (cpu::write-memory 0 #x9A)  ; TXS
    (cpu::cpu-step)
    (ok (= #x42 (cpu::cpu-x-register cpu::*default-cpu*)) "X register should be unchanged"))

  (testing "txs - doesn't affect any flags"
    (setup-test-cpu)
    (setf (cpu::cpu-x-register cpu::*default-cpu*) #x80)  ; Negative value
    (cpu::set-flag :zero 1)
    (cpu::set-flag :negative 1)
    (cpu::set-flag :carry 1)
    (cpu::set-flag :overflow 1)
    (cpu::write-memory 0 #x9A)  ; TXS
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be unchanged")
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be unchanged")
    (ok (= 1 (cpu::get-flag :carry)) "Carry flag should be unchanged")
    (ok (= 1 (cpu::get-flag :overflow)) "Overflow flag should be unchanged")))

(deftest test-tya
  (testing "tya - basic transfer"
    (setup-test-cpu)
    (setf (cpu::cpu-y-register cpu::*default-cpu*) #x42)
    (cpu::write-memory 0 #x98)  ; TYA
    (cpu::cpu-step)
    (ok (= #x42 (cpu::cpu-accumulator cpu::*default-cpu*)) "Accumulator should contain Y register value"))

  (testing "tya - preserves y register"
    (setup-test-cpu)
    (setf (cpu::cpu-y-register cpu::*default-cpu*) #x42)
    (cpu::write-memory 0 #x98)  ; TYA
    (cpu::cpu-step)
    (ok (= #x42 (cpu::cpu-y-register cpu::*default-cpu*)) "Y register should be unchanged"))

  (testing "tya - zero flag when transferring zero"
    (setup-test-cpu)
    (setf (cpu::cpu-y-register cpu::*default-cpu*) #x00)
    (cpu::write-memory 0 #x98)  ; TYA
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :zero)) "Zero flag should be set when transferring zero")
    (ok (= #x00 (cpu::cpu-accumulator cpu::*default-cpu*)) "Accumulator should be zero"))

  (testing "tya - negative flag when transferring negative value"
    (setup-test-cpu)
    (setf (cpu::cpu-y-register cpu::*default-cpu*) #x80)
    (cpu::write-memory 0 #x98)  ; TYA
    (cpu::cpu-step)
    (ok (= 1 (cpu::get-flag :negative)) "Negative flag should be set when transferring negative value")
    (ok (= #x80 (cpu::cpu-accumulator cpu::*default-cpu*)) "Accumulator should have high bit set")))
