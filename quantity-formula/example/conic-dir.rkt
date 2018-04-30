#lang sweet-exp agile

(provide (all-defined-out))

(require racket/math
         racket/set
         "../quantity-formula.rkt"
         "conic.rkt"
         "dir.rkt")
(module+ test
  (require rackunit))

;; --------------------------------------------------------------

declare-quantity plane-dir (object)
declare-quantity plane-dir-normalized (object)

declare-quantity eccentricity-dir (object)
declare-quantity eccentricity-dir-normalized (object)

declare-quantity perifocus-dir (object)
declare-quantity apofocus-dir (object)

;; r@r^ : Dir -> Dir
;; warning: the output will be infinite for the ends of a
;;          parabola
;; warning: the output will be infinite for the ends of a
;;          hyperbola and negative for the "mirror" side
declare-quantity radius@dir (object)

;; --------------------------------------------------------------

define-formula ed->e (object)
  #:input ([ed (eccentricity-dir object)])
  #:output ([e (eccentricity object)]
            [e^ (eccentricity-dir-normalized object)])
  e = (dir-dist ed)
  e^ = (dir-normalize/else ed (λ () +x))

define-formula e^apoperi->apodperid (object)
  #:input ([e^ (eccentricity-dir-normalized object)]
           [peri (perifocus object)]
           [apo (apofocus object)])
  #:output ([perid (perifocus-dir object)]
            [apod (apofocus-dir object)])
  perid = (dir-scale e^ peri)
  apod = (dir-scale e^ (- apo))

define-formula p^e^r@θ->r@r^ (object)
  #:input ([p^ (plane-dir-normalized object)]
           [e^ (eccentricity-dir-normalized object)]
           [r@θ (radius@theta object)])
  #:output ([r@r^ (radius@dir object)])
  r@r^ = (λ (r^)
           (let* ([r^ (dir-project/plane r^ p^)]
                  ;; e^ is pointing "back"
                  ;; l^ is pointing "left"
                  ;; p^ is pointing "up"
                  ;[l^ (dir-cross e^ p^)]
                  [θ (dir-angle-between r^ e^)])
             (r@θ θ)))

;; --------------------------------------------------------------

