#lang racket

(require "pentolla.rkt")

(provide
 (contract-out
  [choose-action (->i ([s (and/c state?
                                 (not/c state-game-over?))])
                      [result (s) (and/c action?
                                         (lambda (action)
                                           (state-action-legal? s action)))])]))

(define (choose-action state)
  (let ((actions (state-legal-actions state)))
    (list-ref actions (random (length actions)))))
