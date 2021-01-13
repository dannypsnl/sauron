#lang s-exp framework/keybinding-lang

(require "panel/version-control.rkt"
         "starter.rkt"
         "project-manager.rkt")

(define (c+ k)
  (match (system-type 'os)
    ;; `d` is command
    ['macosx (format "d:~a" k)]
    ;; `c` is ctrl
    [_ (format "c:~a" k)]))
(define (o+ k)
  (match (system-type 'os)
    ;; `a` is option
    ['macosx (format "a:~a" k)]
    ;; `~c` is alt
    [_ (string-append "~c:" k)]))
(define (send-command command editor event)
  (send (send editor get-keymap) call-function
        command editor event #t))
(define (rebind key f)
  (keybinding key f))

;;; c+e run REPL
(rebind (c+ "e")
        (λ (editor event)
          (send-command "run" editor event)))
;;; c+r rename identifier
(rebind (c+ "r")
        (λ (editor event)
          (send-command "Rename Identifier" editor event)))
;;; c+x cut line if no selection, else cut selection
(rebind (c+ "x")
        (λ (editor event)
          (let* ([s (send editor get-start-position)]
                 [e (send editor get-end-position)]
                 [select? (not (= s e))])
            (unless select?
              (let* ([start-line (send editor position-line (send editor get-start-position))]
                     [end-line (send editor position-line (send editor get-end-position))]
                     [start (send editor line-start-position start-line)]
                     [end (send editor line-end-position end-line)])
                (send editor set-position start end)))
            (send-command "cut-clipboard" editor event))))
;;; c+b jump to definition
(define (jump-to-definition editor event)
  (send-command "Jump to Definition (in Other File)" editor event)
  (send-command "Jump to Binding Occurrence" editor event))
(keybinding (c+ "b") jump-to-definition)
(keybinding (c+ "leftbutton")
            ;; TODO: improvement, use mouse position, not cursor position
            jump-to-definition)

;;; delete whole thing from current position to the start of line
(keybinding (c+ "backspace")
            (λ (editor event)
              (define end (send editor get-start-position))
              (define line (send editor position-line end))
              (define start (send editor line-start-position line))
              (send editor delete start end)))

;;; delete previous sexp
(keybinding (o+ "backspace")
            (λ (editor event)
              (define cur-pos (send editor get-start-position))
              (define pre-sexp-pos (send editor get-backward-sexp cur-pos))
              ; ensure pre-sexp existed
              (when pre-sexp-pos
                (send editor delete pre-sexp-pos cur-pos))))

;;; comment/uncomment selected text, if no selected text, target is current line
(keybinding (c+ "semicolon")
            (λ (editor event)
              ; NOTE: get-start-position and get-end-position would have same value when no selected text
              ; following code comment all lines of selected text(or automatically select cursor line)
              (let* ([start-line (send editor position-line (send editor get-start-position))]
                     [end-line (send editor position-line (send editor get-end-position))]
                     [start (send editor line-start-position start-line)]
                     [end (send editor line-end-position end-line)]
                     [selected-text (send editor get-text start end)])
                (if (string-prefix? selected-text ";")
                    (send editor uncomment-selection start end)
                    (send editor comment-out-selection start end)))))

(keybinding "(" (λ (editor event) (send-command "insert-()-pair" editor event)))
(keybinding "[" (λ (editor event) (send-command "insert-[]-pair" editor event)))
(keybinding "{" (λ (editor event) (send-command "insert-{}-pair" editor event)))
(keybinding "\"" (λ (editor event) (send-command "insert-\"\"-pair" editor event)))

(keybinding (c+ "k")
            (λ (editor event)
              (define frame (new frame%
                                 [label "Version Control: Commit"]
                                 [width 300]
                                 [height 600]))
              (define vc (new version-control% [parent frame]))
              (send frame center)
              (send frame show #t)))

(keybinding (c+ "m")
            (λ (editor event)
              (new starter%
                   [label "select a project"]
                   [width 300]
                   [height 300]
                   [open-ide
                    (λ (path)
                      (current-project path))])))