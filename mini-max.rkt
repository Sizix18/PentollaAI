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
  (minimax state 3 true))

(define (minimax node depth max-player)
  (let ((max-best 0)
        (min-best 10))
  (cond
    [(or (= 0 depth) (empty? (state-children node))
         (heuristic node))]
    [(eq? true max-player)
     (map (lambda (child)
          (max((minimax child (- depth 1) false) max-best)
              (state-children node))))]
    [else
     (map (lambda (child)
          (min((minimax child (- depth 1) false) min-best)
              (state-children node))))])))

(define (heuristic n)
  #|heuristic for (first played list)
  (* (tile-max-x ) (tile-max-y )
   )|#
  10
  )