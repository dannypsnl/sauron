#lang racket/gui
;;; Follow the color scheme DrRacket is using (Preferences | Colors | Color Schemes),
;;; so Sauron's panels don't stay white under a dark scheme.
(provide themed-hierarchical-list%
         themed-editor-canvas%
         themed-item-style-list)

(require framework
         mrlib/hierlist)

; hierlist hands every item's editor a fresh style list, so items never pick up
; `editor:set-default-font-color`. Pointing them all at one shared list means a single
; delta recolors every item, including the ones already on screen.
(define themed-item-style-list (new style-list%))
; a fresh style-list% only holds "Basic"; editors would create "Standard" on demand, but we
; need it to exist up front so the first color callback has something to recolor
(define themed-item-style
  (send themed-item-style-list new-named-style
        "Standard"
        (send themed-item-style-list basic-style)))

(define (set-text-color! c)
  (define d (make-object style-delta%))
  (send d set-delta-foreground c)
  (send themed-item-style set-delta d))

(set-text-color! (color-prefs:lookup-in-color-scheme 'framework:default-text-color))
(color-prefs:register-color-scheme-entry-change-callback
 'framework:default-text-color
 set-text-color!)

; canvas:color-mixin tracks 'framework:basic-canvas-background for us
(define themed-hierarchical-list%
  (canvas:color-mixin (canvas:basic-mixin hierarchical-list%)))
(define themed-editor-canvas%
  (canvas:color-mixin (canvas:basic-mixin editor-canvas%)))
