#lang racket

(require goblins
         goblins/actor-lib/methods
         goblins/actor-lib/bootstrap

         "mirto-actors.rkt")




;; the actor making high-level decisions

(define (square-side remaining motors)
  (lambda (distance)
    (if (> remaining 0)
        (on (<- motors 'start-forward 100000 200)
            (lambda (distance)
              (on (<- motors 'start-rotate 90 200)
                  (square-side (- remaining 1) motors))))
        #f)))

(define (^robot bcom motors)
  (lambda ()
    ((square-side 4 motors) 0)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup and time ticking ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (start-loop time-step)
  (define control-vat (make-vat))
  (define-vat-run c/run control-vat)

  (define plan-vat (make-vat))
  (define-vat-run p/run plan-vat)

  (open-asip)

  (define lcd (c/run (spawn ^lcd)))
  (define motors (c/run (spawn ^motors lcd)))
  (p/run (<- (spawn ^robot motors)))
  (let loop ()
    (c/run
     (<- motors 'tick (current-milliseconds)))
    (sleep time-step)
    (loop)))
  



(start-loop .1)
