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

(define latex-complete
  #hash(("\\" . "\\")
        ;;; logic
        ("forall" . "∀") ("all" . "∀")
        ("ex" . "∃") ("nex" . "∄")
        ("!=" . "≠") ("neq" . "≠") ("==" . "≡") ("equiv" . "≡") ("~=" . "≌") ("cong" . "≌")
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
        ("->" . "→") ("to" . "→") ("=>" . "⇒")
        ("<-" . "←") ("<=" . "⇐")
        ("<->" . "↔") ("<=>" . "⇔")
        ("m>" . "↦") ("-->" . "⟶")
        ("u>" . "↑") ("U>" . "⇑")
        ("d>" . "↓") ("D>" . "⇓")
        ;;; greek
        ("ga" . "α") ("alpha" . "α")
        ("gb" . "β") ("beta" . "β")
        ("gd" . "δ") ("Gd" . "Δ")
        ("ge" . "ε")
        ("gg" . "γ") ("Gg" . "Γ") ("Gamma" . "Γ")
        ("gh" . "η")
        ("gi" . "ι")
        ("gk" . "κ")
        ("gl" . "λ") ("lam" . "λ") ("lambda" . "λ") ("Gl" . "Λ")
        ("gm" . "μ")
        ("gn" . "ν")
        ("go" . "ω") ("Go" . "Ω")
        ("gp" . "π") ("pi" . "π") ("Gp" . "Π") ("Pi" . "Π")
        ("gr" . "ρ")
        ("gs" . "σ") ("Gs" . "Σ")
        ("gt" . "τ")
        ("gv" . "ν") ("Gv" . "Υ")
        ("gw" . "ϕ") ("Gw" . "Φ")
        ("gx" . "χ")
        ("gy" . "ψ") ("Gy" . "Ψ")
        ("gz" . "ζ") ("zeta" . "ζ")
        ;;; arith
        ("times" . "×")
        ("div" . "÷")
        ;;; misc
        (":" . "∶")
        ("::" . "∷")))
