#lang sweet-exp agile

(require racket/math
         racket/set
         "../quantity-formula.rkt")
(module+ test
  (require rackunit))

;; Quantities for Conic Sections:
;;  - Eccentricity
;;  - Semi-Latus Rectum
;;  - Semi-Major Axis
;;  - Semi-Minor Axis

;; e
declare-quantity eccentricity (object)

;; l
declare-quantity semi-latus-rectum (object)

;; a
;; warning: will be infinite for a parabola
;;          NOT the same as the `a` in the quadradic formula
declare-quantity semi-major-axis (object)

;; b
;; TODO: does this make sense for a parabola?
declare-quantity semi-minor-axis (object)

;; c
;; warning: will be infinite for a parabola
declare-quantity linear-eccentricity (object)

;; p
;; warning: will be infinite for a circle
declare-quantity focal-parameter (object)

;; r@θ : Angle -> Real
;; warning: the output will be infinite for the ends of a
;;          parabola
;; warning: the output will be infinite for the ends of a
;;          hyperbola and negative for the "mirror" side
declare-quantity radius@theta (object)

;; peri
declare-quantity perifocus (object)

;; apo
;; warning: will be infinite for a parabola
;; warning: will be negative for the "mirror" side of a
;;          hyperbola
declare-quantity apofocus (object)

;; ----------------------------------------------------

;; The whole shape of the conic is determined by just
;; e and l

define-formula el->r@θ (object)
  #:input ([e (eccentricity object)]
           [l (semi-latus-rectum object)])
  #:output ([r@θ (radius@theta object)])
  r@θ = (λ (θ) (/ l
                  {1 + {e * (cos θ)}}))

;; Computing e from other quantities

define-formula ac->e (object)
  #:input ([a (semi-major-axis object)]
           [c (linear-eccentricity object)])
  #:output ([e (eccentricity object)])
  e = {c / a}

;; Computing l from other quantities

define-formula pe->l (object)
  #:input ([p (focal-parameter object)]
           [e (eccentricity object)])
  #:output ([l (semi-latus-rectum object)])
  l = {p * e}

define-formula eperi->l (object)
  #:input ([e (eccentricity object)]
           [peri (perifocus object)])
  #:output ([l (semi-latus-rectum object)])
  l = {peri * {1 + e}}

define-formula ae->l (object)
  #:input ([a (semi-major-axis object)]
           [e (eccentricity object)])
  #:output ([l (semi-latus-rectum object)])
  l = {a * {1 - (sqr e)}}

define-formula eapo->l (object)
  #:input ([e (eccentricity object)]
           [apo (apofocus object)])
  #:output ([l (semi-latus-rectum object)])
  l = {apo * {1 - e}}

;; Computing apo and peri from other quantities

define-formula el->apoperi (object)
  #:input ([e (eccentricity object)]
           [l (semi-latus-rectum object)])
  #:output ([peri (perifocus object)]
            [apo (apofocus object)])
  peri = {l / {1 + e}}
  apo  = {l / {1 - e}}

define-formula ac->apoperi (object)
  #:input ([a (semi-major-axis object)]
           [c (linear-eccentricity object)])
  #:output ([peri (perifocus object)]
            [apo (apofocus object)])
  peri = {a - c}
  apo  = {a + c}

define-formula ae->apoperi (object)
  #:input ([a (semi-major-axis object)]
           [e (eccentricity object)])
  #:output ([peri (perifocus object)]
            [apo (apofocus object)])
  peri = {a * {1 - e}}
  apo  = {a * {1 + e}}

;; Computing c from other quantities

define-formula ae->c (object)
  #:input ([a (semi-major-axis object)]
           [e (eccentricity object)])
  #:output ([c (linear-eccentricity object)])
  c = {a * e}

define-formula aperi->c (object)
  #:input ([a (semi-major-axis object)]
           [peri (perifocus object)])
  #:output ([c (linear-eccentricity object)])
  c = {a - peri}

define-formula apoperi->c (object)
  #:input ([apo (apofocus object)]
           [peri (perifocus object)])
  #:output ([c (linear-eccentricity object)])
  c = {1/2 * {apo - peri}}

;; Computing p from other quantities

define-formula el->p (object)
  #:input ([e (eccentricity object)]
           [l (semi-latus-rectum object)])
  #:output ([p (focal-parameter object)])
  p = {l / e}

;; Computing a from other quantities

define-formula ec->a (object)
  #:input ([e (eccentricity object)]
           [c (linear-eccentricity object)])
  #:output ([a (semi-major-axis object)])
  a = {c / e}

define-formula eperi->a (object)
  #:input ([e (eccentricity object)]
           [peri (perifocus object)])
  #:output ([a (semi-major-axis object)])
  a = {peri / {1 - e}}

define-formula eapo->a (object)
  #:input ([e (eccentricity object)]
           [apo (apofocus object)])
  #:output ([a (semi-major-axis object)])
  a = {apo / {1 + e}}

define-formula cperi->a (object)
  #:input ([c (linear-eccentricity object)]
           [peri (perifocus object)])
  #:output ([a (semi-major-axis object)])
  a = {peri + c}

;; ---------------------------------------------------------

(module+ test
  (define o 'o)
  (define G
    (formulas->graph
     (list (el->r@θ o)
           (ac->e o)
           (pe->l o)
           (eperi->l o)
           (ae->l o)
           (eapo->l o)
           (el->apoperi o)
           (ac->apoperi o)
           (ae->apoperi o)
           (el->p o)
           (ae->c o)
           (aperi->c o)
           (apoperi->c o)
           (ec->a o)
           (cperi->a o)
           (eperi->a o)
           (eapo->a o)
           )))

  (define path-apoperi->ae
    (find-formula-path G
                       (set (apofocus o)
                            (perifocus o))
                       (set (eccentricity o)
                            (semi-major-axis o)
                            (radius@theta o))))

  (check-equal? path-apoperi->ae
                (list (apoperi->c o)
                      (cperi->a o)
                      (ac->e o)
                      (ae->l o)
                      (el->r@θ o)))
  )

