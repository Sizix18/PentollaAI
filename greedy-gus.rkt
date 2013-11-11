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
  (let-values (((moves passes) (partition move? (state-legal-actions state))))
    (if (not (empty? moves))
        (first moves)
        (first passes))))
