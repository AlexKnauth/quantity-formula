#lang agile

(provide (struct-out dir)
         +x +y +z
         ;; addition
         dir+ dir-
         ;; dir products
         dir-scale
         dir-dot
         ;; distances
         dir-dist dir-dist^2
         ;; normalizing
         dir-normalize
         dir-normalize/else
         ;; projecting
         dir-project/line
         dir-project/plane
         ;; angles
         dir-angle-between
         )

(require racket/bool
         racket/math)
(module+ test
  (require rackunit))

;; --------------------------------------------------------------

(struct dir [dx dy dz] #:transparent)

(define dir-zero (dir 0 0 0))
(define +x (dir 1 0 0))
(define +y (dir 0 1 0))
(define +z (dir 0 0 1))

;; ----------------------
;; Addition

;; Dir Dir -> Dir
(define (dir+ a b)
  (match* [a b]
    [[(dir ax ay az) (dir bx by bz)]
     (dir (+ ax bx) (+ ay by) (+ az bz))]))

;; Dir Dir -> Dir
(define (dir- a b)
  (match* [a b]
    [[(dir ax ay az) (dir bx by bz)]
     (dir (- ax bx) (- ay by) (- az bz))]))

;; ----------------------
;; Dir Products

;; Dir Real -> Dir
(define (dir-scale dv s)
  (match dv
    [(dir x y z)
     (dir (* x s) (* y s) (* z s))]))

;; Dir Dir -> Real
(define (dir-dot a b)
  (match* [a b]
    [[(dir ax ay az) (dir bx by bz)]
     (+ (* ax bx) (* ay by) (* az bz))]))

;; ----------------------
;; Distances

;; Dir -> NonNegReal
(define (dir-dist^2 dv)
  (match dv
    [(dir x y z) (+ (sqr x) (sqr y) (sqr z))]))

;; Dir -> NonNegReal
(define (dir-dist dv)
  (sqrt (dir-dist^2 dv)))

;; ----------------------
;; Normalizing

;; Dir [-> X] -> (U Dir X)
(define (dir-normalize/else dv els)
  (define dst (dir-dist dv))
  (cond
    [(zero? dst) (els)]
    [else (dir-scale dv (/ dst))]))

;; Dir -> (U DirNormalized #false)
(define (dir-normalize dv)
  (dir-normalize/else dv (λ () #false)))

;; ----------------------
;; Projecting

;; Dir Dir -> Dir
(define (dir-project/line dv line-dir)
  (define line^ (dir-normalize line-dir))
  (cond
    [(false? line^) dir-zero]
    [else
     (define dist (dir-dot dv line^))
     (dir-scale line^ dist)]))

;; Dir Dir -> Dir
(define (dir-project/plane dv plane-dir)
  ;; dv = result + onto-line
  ;; result = dv - onto-line
  (define onto-line (dir-project/line dv plane-dir))
  (dir- dv onto-line))

;; ----------------------
;; Angles

;; An Angle is a Real representing an angle in radians

;; Dir Dir -> Angle
(define (dir-angle-between a b)
  (define cosθ (/ (dir-dot a b) (dir-dist a) (dir-dist b)))
  (acos cosθ))

;; --------------------------------------------------------------

(module+ test
  (test-case "projecting onto a line"
    (check-equal? (dir-project/line (dir 3 4 5) +x) (dir 3 0 0))
    (check-equal? (dir-project/line (dir 3 4 5) +y) (dir 0 4 0))
    (check-equal? (dir-project/line (dir 3 4 5) +z) (dir 0 0 5)))
  (test-case "projecting onto a plane"
    (check-equal? (dir-project/plane (dir 3 4 5) +z)
                  (dir 3 4 0))
    (check-equal? (dir-project/plane (dir 6 7 8) (dir 0 3 0))
                  (dir 6 0 8)))
  )

;; --------------------------------------------------------------

