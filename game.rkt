#lang racket

;;;; game.rkt
;;;; CPSC 481 - Fall 2013
;;;; Project 2 - state space search
;;;;
;;;; In case it matters, this file is licensed according to the BSD
;;;; 2-clause license:
;;;;
;;;; Copyright (c) 2013, Kevin Wortman
;;;; All rights reserved.
;;;; 
;;;; Redistribution and use in source and binary forms, with or
;;;; without modification, are permitted provided that the following
;;;; conditions are met:
;;;; 
;;;;     Redistributions of source code must retain the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer.
;;;; 
;;;;     Redistributions in binary form must reproduce the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer in the documentation and/or other materials
;;;;     provided with the distribution.
;;;; 
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
;;;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
;;;; USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
;;;; AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;;;; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;; POSSIBILITY OF SUCH DAMAGE.

(require racket/sandbox srfi/13 srfi/26 "pentolla.rkt")

(provide

 ;; This is the high level interface that you are intended to use. The syntax
 ;;
 ;; (game-run blue-module-path orange-module-path)
 ;;
 ;; loads Pentolla Duo player modules from blue-module-path and
 ;; orange-module-path, simulates a game between them until it is
 ;; finished, and prints a transcript of the game to standard
 ;; output.
 ;; 
 ;; If a player procedure returns something other than a legal action
 ;; object, or runs longer than the value of time-limit (currently 180
 ;; seconds, or 3 minutes), that player is disqualified and loses.
 ;;
 ;; For technical reasons related to the module import system,
 ;; game-run must be syntax (not a procedure); and blue-module-path
 ;; and orange-module-path must both be literal strings, not
 ;; expressions that yield strings.
 game-run

 (contract-out
  [struct game ((blue-name string?)
                (orange-name string?)
                (state state?)
                (dq (or/c false? player-color?))
                (turns exact-nonnegative-integer?)
                (log (listof string?)))]

  [game-over? (-> game? boolean?)]
  
  [game-tied? (-> (and/c game? game-over?) boolean?)]
  [game-won? (-> (and/c game? game-over?) boolean?)]
  
  [game-winner (-> (and/c game? game-won?) player-color?)]
  
  [game-transcript (-> (and/c game? game-over?) string?)]))
  
(struct game (blue-name orange-name state dq turns log) #:prefab)

(define (game-over? game)
  (or (game-dq game)
      (state-game-over? (game-state game))))

(define (game-score/blue game)
  (state-score (game-state game) 'blue))

(define (game-score/orange game)
  (state-score (game-state game) 'orange))

(define (game-tied? game)
  (and (not (game-dq game))
       (= (game-score/blue game)
          (game-score/orange game))))

(define game-won? (negate game-tied?))

(define (game-winner game)
  (if (> (game-score/blue game)
         (game-score/orange game))
      'blue
      'orange))

(define (game-transcript g)
  (string-append* (reverse (map (cut string-append <> "\n") (game-log g)))))

(define (player-color-flip color)
  (if (symbol=? 'blue color)
      'orange
      'blue))

(define time-limit 180)

(define-syntax-rule (game-run blue-module-path orange-module-path)
  (begin
    (local-require (rename-in (file blue-module-path)
                              (choose-action blue:choose-action)))
    (local-require (rename-in (file orange-module-path)
                              (choose-action orange:choose-action)))
    
    (define (action-or-violation g)
      (let ((move-proc (if (blue? (state-whose-turn (game-state g)))
                           blue:choose-action
                           orange:choose-action)))
        (with-handlers ([exn:fail:resource?
                         (const "time limit exceeded")])
          (with-limits time-limit #f
            (move-proc (game-state g))))))
    
    (define (log/string g s)
      (display s)
      (newline)
      (struct-copy game g [log (cons s (game-log g))]))
    
    (define (log/last g)
      (log/string g (if (game-tied? g)
                        "TIE GAME"
                        (string-append (string-upcase (symbol->string (game-winner g)))
                                       " WON")))
      (void))
    
    (define (log/dq g violation)
      (log/last (log/string g (string-append (symbol->string (state-whose-turn (game-state g)))
                                             " is DISQUALIFIED due to violation: "
                                             violation))))
    
    (define (action->string player a)
      (string-append
       "  " (symbol->string player)
       (if (pass? a)
           " PASSES"
           (string-append " moves a "
                          (number->string (tile-length (move-tile a)))
                          "-omino to ("
                          (number->string (move-x a))
                          ", "
                          (number->string (move-y a))
                          ") with "
                          (if (= 1 (move-rotations a))
                              "1 rotation"
                              (string-append (number->string (move-rotations a))
                                             " rotations"))))))
    
    (define (board->string state)
      (define (color-at x y)
        (let ((t (findf (cut tile-overlaps/posn? <> (posn x y))
                        (state-played state))))
          (cond
            ((not t) ".")
            ((tile-blue? t) "B")
            (else "O"))))
      
      (string-append*
       (for/list [(y (in-range 1 (add1 board-dim)))]
         (string-append*
          "\n  "
          (for/list [(x (in-range 1 (add1 board-dim)))]
            (color-at x y))))))
    
    (define (turn g)
      (if (game-over? g)
          (log/last g)
          (let* ((g (log/string g (string-append "TURN "
                                                 (number->string (game-turns g)))))
                 (old-state (game-state g))
                 (action (action-or-violation g)))
            (cond
              [(not (action? action))
               (log/dq g action)]
              [(state-action-violation old-state action)
               (log/dq g (state-action-violation old-state action))]
              [else
               (let* ((g (log/string g (action->string (state-whose-turn old-state) action)))
                      (new-state (state-transition old-state action))
                      (g (log/string g (board->string new-state))))
                 (turn (struct-copy game g
                                    [state new-state]
                                    [turns (add1 (game-turns g))])))]))))
    
    (turn (game blue-module-path orange-module-path start-state #f 1 empty))))
