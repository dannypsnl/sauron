#lang s-exp framework/keybinding-lang

(require net/sendurl
         syntax/parse/define
         sauron/jump-to-def
         sauron/meta
         sauron/collect/api
         sauron/version-control/pusher
         sauron/version-control/panel
         sauron/project/manager)

(define-syntax-parser cmd/ctrl+
  [(_ key fn) #'(keybinding (c+ key) fn)])
(define-syntax-parser opt/alt+
  [(_ key fn) #'(keybinding (o+ key) fn)])

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
    ;; `~c:m` is alt or meta
    [_ (string-append "~c:m:" key)]))
(define (send-command command editor event)
  (send (send editor get-keymap) call-function command editor event #t))

;;; c+e run REPL
(cmd/ctrl+ "e" (λ (editor event) (send-command "run" editor event)))
;;; c+enter run selected, enclosed, or nearest expression
(cmd/ctrl+ "enter" (λ (editor _event)
  (define shift-focus? #f)
  (define sp (send editor get-start-position))
  (cond
    [(not (= sp (send editor get-end-position)))
      ;; we have a selection, send that to REPL
      (send-range-to-repl editor (send editor get-start-position) (send editor get-end-position) shift-focus?)]
    [(send editor find-up-sexp sp)
      ;; we are inside some expression;
      ;; find the enclosing expression
      (define pos (send editor find-up-sexp sp))
      (send-range-to-repl editor
                          pos
                          (send editor get-forward-sexp pos)
                          shift-focus?)]
    [else
      ;; we are in the top-level, try best to find a next (foward) or previous (backward) expression
      (define fw (send editor get-forward-sexp sp))
      (define bw (send editor get-backward-sexp sp))
      (cond
        ; no proper expression can be sent to REPL, do nothing
        [(and (not fw) (not bw)) (void)]
        [(not fw) (send-range-to-repl editor bw (send editor get-forward-sexp bw) shift-focus?)]
        [else (send-range-to-repl editor (send editor get-backward-sexp fw) fw shift-focus?)])])))

(define (send-range-to-repl editor start end shift-focus?)
  #|
  start to end is the fragment (should be an expression) be sent to REPL
  shift-focus? is #t, the focus will move to REPL panel
  |#
  (unless (= start end) ;; don't send empty regions
    (define ints (send (send editor get-tab) get-ints))
    (define frame (send (send editor get-tab) get-frame))
    ;; copy the expression over to the interactions window
    (send editor move/copy-to-edit 
          ints start end
          (send ints last-position)
          #:try-to-move? #f)
    
    ;; erase any trailing whitespace
    (let loop ()
      (define last-pos (- (send ints last-position) 1))
      (when (last-pos . > . 0)
        (define last-char (send ints get-character last-pos))
        (when (char-whitespace? last-char)
          (send ints delete last-pos (+ last-pos 1))
          (loop))))
    
    ;; put back a single newline
    (send ints insert
          "\n"
          (send ints last-position)
          (send ints last-position))
    
    ;; make sure the interactions is visible 
    ;; and run the submitted expression
    (send frame ensure-rep-shown ints)
    (when shift-focus? (send (send ints get-canvas) focus))
    (send ints do-submission)))

;;; c+r rename identifier
(cmd/ctrl+ "r" (λ (editor event) (send-command "Rename Identifier" editor event)))
;;; c+s save file
(cmd/ctrl+ "s"
           (λ (editor event)
             (when (object-method-arity-includes? editor 'set-needs-execution-message 1)
               (define filename (send editor get-filename))
               (if filename
                   ; if file exists
                   (send editor save-file)
                   ; else invoke the finder helper to store file
                   (let ([project-dir (preferences:get 'current-project)])
                     (define filename (if project-dir
                                          (finder:put-file "Untitled" project-dir)
                                          (finder:put-file)))
                     (send editor save-file filename)
                     )))))
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
;;; c+b
; 1. jump to definition
; 2. show references of current definition
(define (jump-to-def editor event)
  (jump-add! (send editor get-tab) (send editor get-start-position))
  (define filename (send editor get-filename))
  (define start-pos (send editor get-start-position))
  (cond
    [(and filename (get-def filename start-pos))
      (define id (get-def filename start-pos))
      (show-references editor filename id)]
    [else
      (and
        (send-command "Jump to Definition (in Other File)" editor event)
        (send-command "Jump to Binding Occurrence" editor event))]))
(cmd/ctrl+ "b" jump-to-def)
(cmd/ctrl+ "leftbutton" jump-to-def)
(cmd/ctrl+ "s:b"
           (λ (editor event)
             (match (jump-pop!)
               [#f (void)]
               [(jump-pos tab pos)
                (define frame (send (send editor get-tab) get-frame))
                (send frame change-to-tab tab)
                (define ed (send tab get-defs))
                (send ed set-position pos)])))

;;; c+s+t reopen the recently closed tab
(cmd/ctrl+ "s:t"
           (λ (editor event)
             (send+ editor
                    (get-tab)
                    (get-frame)
                    (reopen-closed-tab))))

;;; delete whole thing from current position to the start of line
(cmd/ctrl+ "backspace"
           (λ (editor event)
             (define end (send editor get-start-position))
             (define line (send editor position-line end))
             (define start (send editor line-start-position line))
             (send editor delete start end)))

;;; delete previous sexp
(opt/alt+
 "backspace"
 (λ (editor event)
   (if (object-method-arity-includes? editor 'get-backward-sexp 1)
       (let* ([cur-pos (send editor get-start-position)]
              [pre-sexp-pos (send editor get-backward-sexp cur-pos)])
         ; ensure pre-sexp existed
         (when pre-sexp-pos
           (send editor delete pre-sexp-pos cur-pos)))
       (send (if (object-method-arity-includes? editor 'get-editor 0) (send editor get-editor) editor)
             delete
             'start))))

;;; comment/uncomment selected text, if no selected text, target is current line
(cmd/ctrl+
 "semicolon"
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

                 (define/augment (on-close) (set! vc-open? #f))))
             (unless vc-open?
               (set! vc-open? #t)
               (set! frame-<?> (new vc-frame%))
               (new version-control% [parent frame-<?>]))
             (when frame-<?>
               (send* frame-<?> [center] [show #t]))))
(cmd/ctrl+ "s:k" (λ (editor event) (make-pusher "push")))
(cmd/ctrl+ "s:p" (λ (editor event) (make-pusher "pull")))

(cmd/ctrl+ "m"
           (λ (editor event)
             (define manager
               (new project-manager%
                    [label "select a project"]
                    [on-select (λ (path) (preferences:set 'current-project path))]))
             (send manager run)))

(cmd/ctrl+ "d"
           (λ (editor event)
             (define filename-<?> (send editor get-filename))
             (when filename-<?>
               ;;; FIXME: this should also works for untitled file
               (define doc-page-<?> (get-doc filename-<?> (send editor get-start-position)))
               (when doc-page-<?>
                 (send-url doc-page-<?> #f)))))

(keybinding "(" (λ (editor event) (send-command "insert-()-pair" editor event)))
(keybinding "[" (λ (editor event) (send-command "insert-[]-pair" editor event)))
(keybinding "{" (λ (editor event) (send-command "insert-{}-pair" editor event)))
(keybinding "\"" (λ (editor event) (send-command "insert-\"\"-pair" editor event)))

(keybinding
 "space"
 (λ (editor event)
   (when (object-method-arity-includes? editor 'get-backward-sexp 1)
     (define end (send editor get-start-position))
     (define start (send editor get-backward-sexp end))
     (when start
       (define to-complete (send editor get-text start end))
       (when (string-prefix? to-complete "\\")
         ;;; select previous sexp
         (send editor set-position start end)
         ;;; replace it with new text
         (send editor
               insert
               (hash-ref latex-complete (string-trim to-complete "\\" #:right? #f) to-complete)))))
   (send (if (object-method-arity-includes? editor 'get-editor 0) (send editor get-editor) editor)
         insert
         " ")))
