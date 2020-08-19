#lang racket

(provide (all-defined-out))

(define control-key-list
  '(#\return #\space #\tab #\backspace
             release start cancel clear
             insert menu escape capital pause
             next end home left
             up down left right
             f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 f24
             numlock
             wheel-up wheel-down wheel-left wheel-right))

(define racket-builtin-form*
  '("(define )"
    "(define () )"
    "(let ([]) )"
    "(lambda () )"
    "(cond
  [else ])"
    "(match 
  [else ])"
    "(require )"
    "(provide )"))

(define latex-complete
  #hash(("\\" . "\\")
        ;;; logic
        ("forall" . "∀") ("all" . "∀")
        ("ex" . "∃") ("nex" . "∄")
        ("neq" . "≠") ("equiv" . "≡") ("cong" . "≌")
        ("land" . "∧") ("lor" . "∨")
        ("neg" . "¬")
        ("top" . "⊤") ("bot" . "⊥")
        ("|-" . "⊢") ("n|-" . "⊬")
        ("-|" . "⊣")
        ("qed" . "∎")
        ;;; set
        ("0" . "∅")
        ("in" . "∈") ("nin" . "∉")
        ;;; arrow
        ("->" . "→") ("=>" . "⇒")
        ("<-" . "←") ("<=" . "⇐")
        ("<->" . "↔") ("<=>" . "⇔")
        ("m>" . "↦") ("-->" . "⟶")
        ("u>" . "↑") ("U>" . "⇑")
        ("d>" . "↓") ("D>" . "⇓")
        ;;; greek
        ("ga" . "α")
        ("gb" . "β")
        ("gd" . "δ") ("Gd" . "Δ")
        ("ge" . "ε")
        ("gg" . "γ") ("Gg" . "Γ")
        ("gh" . "η")
        ("gi" . "ι")
        ("gk" . "κ")
        ("gl" . "λ") ("Gl" . "Λ")
        ("gm" . "μ")
        ("gn" . "ν")
        ("go" . "ω") ("Go" . "Ω")
        ("gp" . "π") ("Gp" . "Π")
        ("gr" . "ρ")
        ("gs" . "σ") ("Gs" . "Σ")
        ("gt" . "τ")
        ("gv" . "ν") ("Gv" . "Υ")
        ("gw" . "ϕ") ("Gw" . "Φ")
        ("gx" . "χ")
        ("gy" . "ψ") ("Gy" . "Ψ")
        ("gz" . "ζ")
        ;;; arith
        ("times" . "×")
        ("div" . "÷")
        ;;; misc
        (":" . "∶")
        ("::" . "∷")))
