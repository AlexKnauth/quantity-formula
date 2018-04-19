#lang racket/base

(provide powerset
         in-powerset
         powerset->list)

(require racket/list
         racket/sequence
         racket/set)

;; powerset : [Setof X] -> [Setof [Setof X]]
(define (powerset s)
  (for/set ([sublist (in-combinations (set->list s))])
    (list->set sublist)))

;; in-powerset : [Setof X] -> [Sequenceof [Setof X]]
(define (in-powerset s)
  (sequence-map list->set (in-combinations (set->list s))))

;; powerset->list : [Setof X] -> [Listof [Setof X]]
(define (powerset->list s)
  (map list->set (combinations (set->list s))))

