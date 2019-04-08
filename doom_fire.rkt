#lang racket

;; http://fabiensanglard.net/doom_fire_psx/index.html

(require 2htdp/image 2htdp/universe)
(require threading match-plus)

(define canvas-width 320)
(define canvas-height 168)

;; should be a vector due to random access
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

(define (compute-line depth previous-line orig-line)
  ;; mapping on multiple lists == zipping in other languages
  ;; zipping with the list moved to the left & right to get
  ;; the previous & next items
  (map
   (λ (previous-before previous previous-after orig-color)
     (define x0
       (case (random 3)
         [(0) previous-before]
         [(2) previous-after]
         [else previous]))
     (define x1
       (if (eq? (random 2) 1)
           (max 0 (sub1 x0))
           x0))
     ;; special case if we compute 0, don't necessarily stay black,
     ;; maybe the flame can keep running if it was running in
     ;; the previous frame. allows the frame to have vertical "holes"
     ;; and for a VERY NICE effect when shutting down the fire from
     ;; the bottom.
     (define x2 (if (eq? x1 0)
                    (case (random 2)
                      [(0) (quotient orig-color 2)]
                      [(1) (sub1 orig-color)])
                    x1))
     (max 0 (min depth x2)))
   (append
    (list (first previous-line))
    (drop-right previous-line 1)) ;; drop-right & append not ideal for perf
   previous-line
   (append
    (drop previous-line 1)
    (list (first previous-line)))
   orig-line)) ;; append not ideal for perf

(define/match* (compute-next-frame (st depth orig-frame))
  (define new-start
    (for/fold
     ([frame (list (first orig-frame))])
     ([orig-line orig-frame])
      (cons (compute-line depth (first frame) orig-line) frame)))
  (define new-frame (reverse
                     (if (>= (length new-start) (length orig-frame))
                         new-start
                         (append
                          (reverse (drop orig-frame (length new-start)))
                          new-start))))
  (st (add1 depth) new-frame))

(define (paint-line line start-canvas)
  (for/fold
   ([canvas start-canvas])
   ([pixel line] [x (in-naturals)])
    (cons (list-ref colors pixel) canvas)))

(define/match* (draw-flames (st _ frame))
  (define start-canvas (list))
  (define flames
    (~>
     (for/fold
      ([canvas start-canvas])
      ([i (in-naturals)]
       [line frame])
       (paint-line line canvas))
     (color-list->bitmap _ canvas-width (length frame))))
  (define full-canvas
    (rectangle canvas-width canvas-height "solid" "black"))
  (place-image/align
   flames 0
   (- canvas-height (length frame))
   "left" "top" full-canvas))

;; our state will be the rows to be displayed,
;; containing not colors but indexes of colors
;; at the beginning I start with a single line of
;; white
(define (line-col col)
  (build-list canvas-width (const col)))

(define (start-line)
  (append
   (list (line-col (sub1 color-count)))
   (build-list
    (sub1 canvas-height)
    (const (line-col 0)))))

(struct st (depth frame))

(big-bang (st 0 (start-line))
          (on-key (λ (state key)
                    (st
                     (st-depth state)
                     (cons
                      (line-col 0)
                      (drop (st-frame state) 1)))))
          (on-tick compute-next-frame 0.1)
          (on-draw draw-flames))

