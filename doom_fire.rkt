#lang racket

;; http://fabiensanglard.net/doom_fire_psx/index.html

(require 2htdp/image 2htdp/universe)
(require threading)

(define canvas-width 320)
(define canvas-height 168)

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

(define (compute-line depth previous-line)
  ;; mapping on multiple lists == zipping in other languages
  ;; zipping with the list moved to the left & right to get
  ;; the previous & next items
  (map (λ (previous-before previous previous-after)
         (define x (case (random 3)
                     [(0) previous-before]
                     [(2) previous-after]
                     [else previous]))
         (if (eq? (random 2) 1)
             (max 0 (if (< (sub1 x) (- color-count depth 1)) 0 (sub1 x)))
             x))
       (append (list (first previous-line)) (drop-right previous-line 1)) ;; drop-right & append not ideal for perf
       previous-line
       (append (drop previous-line 1) (list (first previous-line))))) ;; append not ideal for perf

(define (compute-frame depth)
  (reverse (for/fold ([frame (list (start-line))])
            ([i (in-range 0 (sub1 canvas-height))])
    #:break (andmap zero? (first frame))
    (cons (compute-line depth (first frame)) frame))))

(define (paint-line line y start-canvas)
  (for/fold ([canvas start-canvas]) ([pixel line] [x (in-naturals)])
    (cons (list-ref colors pixel) canvas)))

(define (draw-flames depth)
  (define frame (compute-frame depth))
  (define start-canvas (list))
  (define flames (~>
                  (for/fold ([canvas start-canvas])
                            ([i (in-naturals)]
                             [line frame])
                    (paint-line line (- canvas-height (- depth i)) canvas))
                  (color-list->bitmap _ canvas-width (length frame))))
  (define full-canvas
    (rectangle canvas-width canvas-height "solid" "black"))
  (place-image/align flames 0 (- canvas-height (length frame)) "left" "top" full-canvas))

;; our state will be the rows to be displayed,
;; containing not colors but indexes of colors
;; at the beginning I start with a single line of
;; white
(define (start-line)
  (build-list canvas-width (const (sub1 color-count))))

(big-bang 0
          (on-tick add1 .1) ;; .1 => 10fps (otherwise it's too fast)
          (on-draw draw-flames))

