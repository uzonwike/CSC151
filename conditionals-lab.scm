#lang racket
(require gigls/unsafe)
(define grid (image-new 3 3))
(define canvas (image-new 200 200))
(define picture 
  (image-load "/home/rebelsky/MediaScheme/Images/kitten.png"))

(image-set-pixel! grid 0 0 (irgb 192 0 0))
(image-set-pixel! grid 1 0 (irgb 192 0 192))
(image-set-pixel! grid 2 0 (irgb 0 0 192))
(image-set-pixel! grid 0 1 (irgb 192 192 0))
(image-set-pixel! grid 1 1 (irgb 0 192 192))
(image-set-pixel! grid 2 1 (irgb 255 255 255))
(image-set-pixel! grid 0 2 (irgb 0 192 0))
(image-set-pixel! grid 1 2 (irgb 0 0 0))
(image-set-pixel! grid 2 2 (irgb 192 192 192))

(define irgb-brightness
  (lambda (color)
    (round (* 100 (/ (+ (* 0.30 (irgb-red color))
                        (* 0.59 (irgb-green color))
                        (* 0.11 (irgb-blue color)))
                     255)))))

(define irgb-brighter?
  (lambda (color1 color2)
    (if (< (irgb-brightness color1) (irgb-brightness color2))
        color2
        color1)))

(define grey0 (irgb 0 0 0))
(define grey1 (irgb 96 96 96))
(define grey2 (irgb 192 192 192))
(define grey3 (irgb 255 255 255))


(define irgb-4grey
  (lambda (color)
    (if (< (irgb-brightness color) 25)
        (display 'grey0)
        (if (< (irgb-brightness color) 50)
            (display 'grey1)
            (if (< (irgb-brightness color) 75)
                (display 'grey2)
                (display 'grey3))))))

(define rgb-4grey
  (lambda (color)
    (cond 
      ((< (irgb-brightness color) 25)
       (display 'grey0))
      ((< (irgb-brightness color) 50)
       (display 'grey1))
      ((< (irgb-brightness color) 75)
       (display 'grey2))
      (else
       (display 'grey3)))))

(define image-contains-selection?
  (lambda (img left top width height)
    (if
     (and
      (< left (image-width img))
      (< (abs left) width)
      (< 0 (+ width left))
      (< top (image-height img)))
     #t
     #f)))

(define image-safely-select-ellipse!
  (lambda (img operation left top width height)
    (when
        (image-contains-selection? img left top width height)
      (image-select-ellipse! img operation left top width height))))

(define render!
  (lambda (drawing image)
    (context-set-fgcolor! (drawing-color drawing))
    (context-set-brush! "Pixel (1x1 square)")
    (cond
      [(equal? (drawing-type drawing) 'rectangle)
       (image-stroke-selection!
        (image-select-rectangle! image REPLACE
                                (drawing-left drawing)
                                (drawing-top drawing)
                                (drawing-width drawing)
                                (drawing-height drawing)))]
      [(equal? (drawing-type drawing) 'ellipse)
       (image-stroke-selection!
       (image-select-ellipse! image REPLACE
                              (drawing-left drawing)
                              (drawing-top drawing)
                              (drawing-width drawing)
                              (drawing-height drawing)))])
    (image-select-nothing!
     (image-fill-selection! image))))

(define d1
  (hshift-drawing
   30
   (vshift-drawing
    40
    (scale-drawing
     50
     (recolor-drawing "red" drawing-unit-square)))))

(define d2
  (vscale-drawing
   120
   (hscale-drawing
    60
    (recolor-drawing "green" drawing-unit-circle))))


