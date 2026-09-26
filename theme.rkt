#lang racket/gui
;;; Follow the color scheme DrRacket is using (Preferences | Colors | Color Schemes),
;;; so Sauron's panels don't stay white under a dark scheme.
(provide themed-editor-canvas%)

(require framework)

; canvas:color-mixin tracks 'framework:basic-canvas-background for us
(define themed-editor-canvas%
  (canvas:color-mixin (canvas:basic-mixin editor-canvas%)))
