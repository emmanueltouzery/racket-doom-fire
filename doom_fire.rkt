#lang racket

(require 2htdp/image 2htdp/universe)
(require threading)

(define canvas-width 200)
(define canvas-height 200)

(define colors
  (list 
   "black"
   (make-color  #x07 #x07 #x07)
   (make-color  #x1F #x07 #x07)
   (make-color  #x2F #x0F #x07)
   (make-color  #x47 #x0F #x07)
   (make-color  #x57 #x17 #x07)
   (make-color  #x67 #x1F #x07)
   (make-color  #x77 #x1F #x07)
   (make-color  #x8F #x27 #x07)
   (make-color  #x9F #x2F #x07)
   (make-color  #xAF #x3F #x07)
   (make-color  #xBF #x47 #x07)
   (make-color  #xC7 #x47 #x07)
   (make-color  #xDF #x4F #x07)
   (make-color  #xDF #x57 #x07)
   (make-color  #xDF #x57 #x07)
   (make-color  #xD7 #x5F #x07)
   (make-color  #xD7 #x5F #x07)
   (make-color  #xD7 #x67 #x0F)
   (make-color  #xCF #x6F #x0F)
   (make-color  #xCF #x77 #x0F)
   (make-color  #xCF #x7F #x0F)
   (make-color  #xCF #x87 #x17)
   (make-color  #xC7 #x87 #x17)
   (make-color  #xC7 #x8F #x17)
   (make-color  #xC7 #x97 #x1F)
   (make-color  #xBF #x9F #x1F)
   (make-color  #xBF #x9F #x1F)
   (make-color  #xBF #xA7 #x27)
   (make-color  #xBF #xA7 #x27)
   (make-color  #xBF #xAF #x2F)
   (make-color  #xB7 #xAF #x2F)
   (make-color  #xB7 #xB7 #x2F)
   (make-color  #xB7 #xB7 #x37)
   (make-color  #xCF #xCF #x6F)
   (make-color  #xDF #xDF #x9F)
   (make-color  #xEF #xEF #xC7)
   "white"))

(define color-count (length colors))

(define (compute-line previous-line)
  (map sub1 previous-line))

;; is there no better way to draw a single pixel!?!?
(define (paint-pixel canvas x y color)
  (add-line canvas x y x y color))

(define (compute-frame depth)
  (for/fold ([frame (list (start-line))])
            ([i (in-range 1 depth)])
    (cons (compute-line (first frame)) frame)))

(define (paint-line line y start-canvas)
  (for/fold ([canvas start-canvas]) ([pixel line] [x (in-naturals)])
    (paint-pixel canvas x y (list-ref colors pixel))))

(define (draw-flames depth)
  (define frame (compute-frame depth))
  (define start-canvas
    (rectangle canvas-width canvas-height "solid" "black"))
  (for/fold ([canvas start-canvas])
            ([i (in-range 0 (min depth color-count))]
             [line frame])
    (paint-line line (- canvas-height (- depth i)) canvas)))

;; our state will be the rows to be displayed,
;; containing not colors but indexes of colors
;; at the beginning I start with a single line of
;; white
(define (start-line)
  (build-list canvas-width (const (sub1 color-count))))

(big-bang 0
          (on-tick (λ (st) (if (< st color-count) (add1 st) st)))
          (on-draw draw-flames))

