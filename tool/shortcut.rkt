#lang s-exp framework/keybinding-lang

(require data/interval-map
         net/sendurl
         syntax/parse/define
         "../binding.rkt"
         "../jump-to-def.rkt"
         "../meta.rkt"
         "../commit-pusher.rkt"
         "../panel/version-control.rkt"
         "../project-manager.rkt"
         "../project/current-project.rkt")

(define-syntax-parser cmd/ctrl+
  [(_ key fn)
   #'(keybinding (c+ key) fn)])
(define-syntax-parser opt/alt+
  [(_ key fn)
   #'(keybinding (o+ key) fn)])

(define (c+ key)
  (match (system-type 'os)
    ;; `d` is command
    ['macosx (format "d:~a" key)]
    ;; `c` is ctrl
    [_ (format "c:~a" key)]))
(define (o+ key)
  (match (system-type 'os)
    ;; `a` is option
    ['macosx (format "a:~a" key)]
    ;; `~c` is alt
    [_ (string-append "~c:" key)]))
(define (send-command command editor event)
  (send (send editor get-keymap) call-function
        command editor event #t))

;;; c+e run REPL
(cmd/ctrl+ "e"
           (λ (editor event)
             (send-command "run" editor event)))
;;; c+r rename identifier
(cmd/ctrl+ "r"
           (λ (editor event)
             (send-command "Rename Identifier" editor event)))
;;; c+x cut line if no selection, else cut selection
(cmd/ctrl+ "x"
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
(define jump-stack '())
(cmd/ctrl+ "b"
           (λ (editor event)
             (send-command "Jump to Definition (in Other File)" editor event)
             (jump-to-definition editor (send editor get-start-position))))
(cmd/ctrl+ "leftbutton"
           (λ (editor event)
             (send-command "Jump to Definition (in Other File)" editor event)
             (jump-to-definition editor
                                 (send editor find-position
                                       (send event get-x)
                                       (send event get-y)))))
(cmd/ctrl+ "s:b"
           (λ (editor event)
             (define pos (jump-pop))
             (when pos
               (send editor set-position pos))))

;;; delete whole thing from current position to the start of line
(cmd/ctrl+ "backspace"
           (λ (editor event)
             (define end (send editor get-start-position))
             (define line (send editor position-line end))
             (define start (send editor line-start-position line))
             (send editor delete start end)))

;;; delete previous sexp
(opt/alt+ "backspace"
          (λ (editor event)
            (define cur-pos (send editor get-start-position))
            (define pre-sexp-pos (send editor get-backward-sexp cur-pos))
            ; ensure pre-sexp existed
            (when pre-sexp-pos
              (send editor delete pre-sexp-pos cur-pos))))

;;; comment/uncomment selected text, if no selected text, target is current line
(cmd/ctrl+ "semicolon"
           (λ (editor event)
             ; NOTE: get-start-position and get-end-position would have same value when no selected text
             ; following code comment all lines of selected text(or automatically select cursor line)
             (let* ([start-line (send editor position-line (send editor get-start-position))]
                    [end-line (send editor position-line (send editor get-end-position))]
                    [start (send editor line-start-position start-line)]
                    [end (send editor line-end-position end-line)]
                    [selected-text (send editor get-text start end)])
               (if (string-contains? selected-text ";")
                   (send editor uncomment-selection start end)
                   (send editor comment-out-selection start end))
               (send editor set-position start))))

(define vc-open? #f)
(define frame-<?> #f)
(cmd/ctrl+ "k"
           (λ (editor event)
             (define vc-frame%
               (class frame%
                 (super-new [label "Version Control: Commit"] [width 300] [height 600])

                 (define/augment (on-close)
                   (set! vc-open? #f))))
             (unless vc-open?
               (set! vc-open? #t)
               (set! frame-<?> (new vc-frame%))
               (define vc (new version-control% [parent frame-<?>]))
               (send frame-<?> center))
             (when frame-<?>
               (send frame-<?> show #t))))
(cmd/ctrl+ "s:k" (λ (editor event) (make-commit-pusher "push")))
(cmd/ctrl+ "s:p" (λ (editor event) (make-commit-pusher "pull")))

(cmd/ctrl+ "m"
           (λ (editor event)
             (new project-manager%
                  [label "select a project"]
                  [on-select
                   (λ (path)
                     (send current-project set path))])))

(cmd/ctrl+ "d"
           (λ (editor event)
             (send editor update-env)
             (define doc-page? (interval-map-ref (send editor get-doc)
                                                 (send editor get-start-position) #f))
             (when doc-page?
               (send-url/file doc-page?))))

(keybinding "(" (λ (editor event) (send-command "insert-()-pair" editor event)))
(keybinding "[" (λ (editor event) (send-command "insert-[]-pair" editor event)))
(keybinding "{" (λ (editor event) (send-command "insert-{}-pair" editor event)))
(keybinding "\"" (λ (editor event) (send-command "insert-\"\"-pair" editor event)))

(keybinding "space"
            (λ (editor event)
              (define end (send editor get-start-position))
              (define start (send editor get-backward-sexp end))
              (when start
                (define to-complete (send editor get-text start end))
                (when (string-prefix? to-complete "\\")
                  ;;; select previous sexp
                  (send editor set-position start end)
                  ;;; replace it with new text
                  (send editor insert
                        (hash-ref latex-complete
                                  (string-trim to-complete "\\" #:right? #f)
                                  to-complete))))
              (send editor insert " ")))
