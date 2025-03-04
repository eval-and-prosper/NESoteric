;; TODO look into package-inferred-system
(defsystem "nesoteric"
  :version "0.0.1"
  :author "Matt Renfro"
  :maintainer "Matt Renfro"
  :mailto "evalandprosper@gmail.com"
  :license "MIT"
  :source-control "https://github.com/eval-and-prosper/NESoteric.git"
  :depends-on (:alexandria)
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "bits-n-bytes")
                 (:file "memory" :depends-on ("mapper"))
                 (:file "cartridge")
                 (:file "mapper" :depends-on ("cartridge"))
                 (:file "cpu")
                 (:file "system"))))
  :description "NES emulator"
  :in-order-to ((test-op (test-op "nesoteric/tests"))))

(defsystem "nesoteric/tests"
  :author "Matt Renfro"
  :license "MIT"
  :depends-on ("nesoteric"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main")
                 (:file "instructions")
                 (:file "addressing")
                 (:file "mapper")))) :description "Test system for nesoteric"
  :perform (test-op (op c) (symbol-call :rove :run c)))
