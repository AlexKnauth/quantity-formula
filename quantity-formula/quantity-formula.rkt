#lang sweet-exp agile

(provide declare-quantity
         define-formula
         formulas->graph
         find-formula-path
         formula-path-state)

(require graph
         racket/hash
         racket/math
         racket/pretty
         racket/sequence
         racket/set
         racket/struct
         "util/set.rkt")

;; Somewhat inspired by Aristarchus:
;;   https://github.com/walkeri/Aristarchus

;; ---------------------------------------------------------

;; A FundamentalDepIndex is any value that can be
;; equal-compared and hashed. It is something that
;; quantities can "depend on", in a way that if it is
;; different, the quantities are treated as if they were
;; "different quantities". For example, the masses of two
;; separate objects A and B should be treated as different
;; quantities, so there should be a FundamenalDepIndex for
;; object A and a different one for objcet B.

;; A Quantity is the result of a function defined by
;; `declare-quantity`.

;; A Formula is the result of a function defined by
;; `define-formula`. It has the form:
;; (formula input output f)
;;   name   : Symbol
;;   fdis   : [Listof FundamentalDepIndex]
;;   input  : [Setof Quantity]
;;   output : [Setof Quantity]
;;   args   : [List X ...]
;;   f      : [State X ... -> State]
(struct formula [name fdis input output args f] #:transparent
  #:property prop:procedure
  (λ (self state)
    (apply (formula-f self) state (formula-args self)))
  #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (self) (formula-name self))
      (λ (self) (formula-fdis self))))])

;; A FormulaPath is a [Listof Formula]
;; where the formulas are applies in order, from left to
;; right, to get from one state to another.

;; A State is a [Hashof Quantity Any].

;; ---------------------------------------------------------

;; Grammar:
;; fundamental-dep-name ::= name
;;
;; A fundamental-dep-name represents something that, if it
;; is different, it is as if it was *different quantity*.
;; For example, a common fundamental-dep-name is `object`,
;; because the mass of object A is a different quantity than
;; the mass of object B.

;; Form:
;; declare-quantity name (fundamental-dep-name ...)
;;
;; Defines `name` as a function that takes the
;; fundamental-dep-names as arguments, and returns a
;; Quantity.

(define-simple-macro
  (declare-quantity name (fundamental-dep-name ...))
  (begin
    (struct name [fundamental-dep-name ...] #:transparent)))

;; Form:
;; define-formula name (fundamental-dep-name ...)
;;   #:input ([in-name in-quantity] ...)
;;   #:output ([out-name out-quantity] ...)
;;   out-name = expr
;;   ...
;;
;; Defines `name` as a function that takes the
;; fundamental-dep-names as arguments, and returns
;; a Formula.

(define-simple-macro
  (define-formula name (fundamental-dep-name ...)
    #:input ([in-name in-quantity] ...)
    #:output ([out-name out-quantity] ...)
    [out-name* = expr]
    ...)
  #:with [in-quantity* ...] (generate-temporaries #'[in-name ...])
  #:with [out-quantity* ...] (generate-temporaries #'[out-name ...])
  #:with [[out-quan*/name ...] ...] #'[[out-quantity* out-name] ...]
  (begin
    (define (f state in-quantity* ... out-quantity* ...)
      (let ([in-name (hash-ref state in-quantity*)] ...)
        (define out-name* expr)
        ...
        (hash-union
         state
         (hash out-quan*/name ... ...))))
    (define (name fundamental-dep-name ...)
      (let ([in-quantity* in-quantity] ... [out-quantity* out-quantity] ...)
        (formula
         'name
         (list fundamental-dep-name ...)
         (set in-quantity* ...)
         (set out-quantity* ...)
         (list in-quantity* ... out-quantity* ...)
         f)))))

;; ---------------------------------------------------------

;; A FormulaGraph is a graph where:
;;   A Vertex is a [Setof Quantity]
;;   An Edge can correspond to a Formula

(struct formula-graph [quantities formulas]
  #:methods gen:graph
  [;; ----------------------------------
   ;; Vertex-Only Operations
   ;; verticies are sets of quantities
   (define (has-vertex? G v)
     (match-define (formula-graph Qs Fs) G)
     (subset? v Qs))
   ;; are these vertices the same set of quantities?
   (define (vertex=? G u v)
     (set=? u v))
   ;; find all verices
   (define (in-vertices G)
     (match-define (formula-graph Qs Fs) G)
     (in-powerset Qs))
   (define (get-vertices G)
     (sequence->list (in-vertices G)))
   ;; ----------------------------------
   ;; Edge Operations
   ;; does an edge exist from u -> v?
   (define (has-edge? G u v)
     (match-define (formula-graph Qs Fs) G)
     (or
      (subset? v u)
      (for/or ([F (in-list Fs)]
               #:when (subset? (formula-input F) u))
        (define output (set-union u (formula-output F)))
        (subset? v output))))
   ;; find all vertices v such that u -> v
   (define (neighbor-set G u)
     (match-define (formula-graph Qs Fs) G)
     (define trivial (powerset u))
     (for/fold ([acc trivial])
               ([F (in-list Fs)]
                #:when (subset? (formula-input F) u))
       (set-union
        acc
        (powerset (set-union u (formula-output F))))))
   (define (get-neighbors G u)
     (set->list (neighbor-set G u)))
   (define (in-neighbors G u)
     (in-set (neighbor-set G u)))
   ])

;; formulas->graph : [Listof Formula] -> DirectedGraph
(define (formulas->graph formulas)
  (define quantities
    (for/fold ([acc (set)])
              ([fmla (in-list formulas)])
      (set-union
       acc
       (formula-input fmla)
       (formula-output fmla))))
  (formula-graph quantities formulas))

;; find-formula-path :
;; FormulaGraph [Setof Quantity] [Setof Quantity] -> FormulaPath
;; Finds a path from start -> end, and produces the formulas
;; needed to get to knowing end from knowing start.
(define (find-formula-path G start end)
  (match-define (formula-graph Qs Fs) G)
  (define Vs
    (fewest-vertices-path G start end))
  (let loop ([Vs Vs])
    (match Vs
      ['() '()]
      [(list V) '()]
      [(list-rest A B _)
       #:when (subset? B A)
       (loop (rest Vs))]
      [(list-rest A B _)
       (define delta (set-subtract B A))
       (define F
         (for/first ([F (in-list Fs)]
                     #:when (subset? (formula-input F) A)
                     #:when (subset? delta (formula-output F)))
           F))
       (unless F (error 'bad))
       (cons F (loop (rest Vs)))])))

;; formula-path-state : State FormulaPath -> State
(define (formula-path-state state path)
  (for/fold ([state state])
            ([formula (in-list path)])
    (formula state)))

;; ---------------------------------------------------------

