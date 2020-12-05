#lang racket

(require goblins
         goblins/actor-lib/methods
         goblins/actor-lib/bootstrap

         "MirtoEmulatorGui.rkt")


(provide open-asip ^noop ^motors ^lcd)

;;;;
; Constants
;;;;

(define right-wheel 0)
(define left-wheel 1)
(define motor-1° 833)
(define counter-interval 1)


;;;;
; Low-level functions
;;;;

(define (rotate/speed speed)
  (setMotors speed (- 0 speed)))



;;;;;;;;;;;;;;;;
;;;; Actors ;;;;
;;;;;;;;;;;;;;;;

;; low level

(define (^noop bcom)
  (define (noop . rest) (values))
  noop)

(define (^motors bcom lcd)
  (enableCounters counter-interval)
  (methods
   [(stop)
    (stopMotors)]
   [(set left right)
    (setMotors left right)]
   [(start-forward distance speed)
    (setMotors speed speed)
    (resetCount right-wheel)
    (match (spawn-promise-cons)
      [(cons vow resolver)
       ($ lcd "wait for it" 0)
       (bcom (^move-stopper bcom resolver lcd right-wheel distance >=))
       ($ lcd "wait for it..." 0)
       vow])]
   [(tick time)
    ($ lcd "^motors" 2)
    ($ lcd (~v (getCount right-wheel)) 4)]))

(define (^lcd bcom)
  setLCDMessage)

(define (^move-stopper bcom resolver lcd wheel final-distance comparison)
  (methods
   [(tick time)
    (let ([current-distance (getCount wheel)])
      ($ lcd "legendary!" 0)
      ($ lcd "^move-stopper" 2)
      ($ lcd (~v current-distance) 4)
      (if (comparison current-distance final-distance)
          (begin
            (stopMotors)
            (bcom (^motors bcom))
            (<- resolver 'fulfill current-distance))
          #f))]))
