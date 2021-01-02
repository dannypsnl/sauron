#lang s-exp framework/keybinding-lang

;;; delete whole thing from current position to the start of line
(keybinding "d:backspace"
            (lambda (editor event)
              (define end (send editor get-start-position))
              (define line (send editor position-line end))
              (define start (send editor line-start-position line))
              (send editor delete start end)))

;;; delete previous sexp
(keybinding "a:backspace"
            (lambda (editor event)
              (define cur-pos (send editor get-start-position))
              (define pre-sexp-pos (send editor get-backward-sexp cur-pos))
              ; ensure pre-sexp existed
              (when pre-sexp-pos
                (send editor delete pre-sexp-pos cur-pos))))

;;; comment/uncomment selected text, if no selected text, target is current line
(keybinding "d:semicolon"
            (lambda (editor event)
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

(define (auto-wrap-with open close)
  (lambda (editor event)
    (let* ([origin-start (send editor get-start-position)]
           [selected-text (send editor get-text origin-start (send editor get-end-position))])
      (send editor insert (string-join (list open (if selected-text selected-text "") close) ""))
      (send editor set-position (+ 1 origin-start)))))
(keybinding "(" (auto-wrap-with "(" ")"))
(keybinding "[" (auto-wrap-with "[" "]"))
(keybinding "{" (auto-wrap-with "{" "}"))
(keybinding "\"" (auto-wrap-with "\"" "\""))
