#lang sweet-exp agile

(require racket/math
         racket/set
         "../quantity-formula.rkt")

declare-quantity time (time-index)

declare-quantity ∆time (time-index0 time-index1)

declare-quantity mass (object)

declare-quantity position (object time-index)
declare-quantity velocity (object time-index)
declare-quantity acceleration (object time-index)

declare-quantity momentum (object time-index)
declare-quantity kinetic-energy (object time-index)
declare-quantity gravitational-potential-energy (object time-index)

;; ---------------------------------------------------------

define ti:always 'always

;; ---------------------------------------------------------

define-formula t0·t1->∆t (ti0 ti1)
  #:input ([t0 (time ti0)]
           [t1 (time ti1)])
  #:output ([∆t (∆time ti0 ti1)])
  ∆t = {t1 - t0}

define-formula ∆t·x0·v0·a->x1·v1 (object ti0 ti1)
  #:input ([∆t (∆time ti0 ti1)]
           [x0 (position object ti0)]
           [v0 (velocity object ti0)]
           [a (acceleration object ti:always)])
  #:output ([x1 (position object ti1)]
            [v1 (velocity object ti1)])
  v1 = {v0 + {a * ∆t}}
  x1 = {x0 + {v0 * ∆t} + {1/2 * a * (sqr ∆t)}}

define-formula m·v->p·KE (object ti)
  #:input ([m (mass object)]
           [v (velocity object ti)])
  #:output ([p (momentum object ti)]
            [KE (kinetic-energy object ti)])
  p = {m * v}
  KE = {1/2 * m * (sqr v)}

;; ---------------------------------------------------------

define object-A 'A
define ti:0 't0
define ti:1 't1

define t0 (time ti:0)
define t1 (time ti:1)
define m_A (mass object-A)
define x_A_0 (position object-A ti:0)
define x_A_1 (position object-A ti:1)
define v_A_0 (velocity object-A ti:0)
define v_A_1 (velocity object-A ti:1)
define a_A (acceleration object-A ti:always)
define KE_A_0 (kinetic-energy object-A ti:0)
define KE_A_1 (kinetic-energy object-A ti:1)

define ∆t_01 (t0·t1->∆t ti:0 ti:1)
define xv_A_1 (∆t·x0·v0·a->x1·v1 object-A ti:0 ti:1)
define pKE_A_0 (m·v->p·KE object-A ti:0)
define pKE_A_1 (m·v->p·KE object-A ti:1)

define state
  (hash t0    0
        t1    1
        m_A   5
        x_A_0 100
        v_A_0 3
        a_A   1)

(pKE_A_1 (xv_A_1 (∆t_01 (pKE_A_0 state))))

define G
  (formulas->graph (list
                    ∆t_01
                    xv_A_1
                    pKE_A_0
                    pKE_A_1))

G

(define path
  (find-formula-path G
                     (set t0 t1 m_A x_A_0 v_A_0 a_A)
                     (set KE_A_0 x_A_1 KE_A_1)))

path

(formula-path-state state path)
;  -->
;(hash
; (kinetic-energy 'A 't0)
; #e22.5
; (mass 'A)
; 5
; (position 'A 't1)
; #e103.5
; (kinetic-energy 'A 't1)
; 40
; (velocity 'A 't1)
; 4
; (∆time 't0 't1)
; 1
; (time 't0)
; 0
; (momentum 'A 't1)
; 20
; (acceleration 'A 'always)
; 1
; (momentum 'A 't0)
; 15
; (position 'A 't0)
; 100
; (velocity 'A 't0)
; 3
; (time 't1)
; 1)

