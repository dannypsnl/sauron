#lang racket/gui

(provide current-project)

(require framework/notify)

(define current-project
  (new notify:notify-box%
       [value #f]))
