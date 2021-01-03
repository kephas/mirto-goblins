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
   [(set right left)
    (setMotors right left)]
   [(start-forward distance speed)
    (setMotors speed speed)
    (resetCount right-wheel)
    (match (spawn-promise-cons)
      [(cons vow resolver)
       (bcom (^move-stopper bcom resolver lcd right-wheel distance >=) vow)])]
   [(start-rotate-left distance speed)
    (setMotors speed (* -1 speed))
    (resetCount left-wheel)
    (match (spawn-promise-cons)
      [(cons vow resolver)
       (bcom (^move-stopper bcom resolver lcd left-wheel distance >=) vow)])]
   [(start-rotate angle speed)
    (let ([distance (* motor-1° angle)]
          [positive? (> angle 0)]
          [speed+ speed]
          [speed- (* -1 speed)])
      (match (spawn-promise-cons)
        [(cons vow resolver)
         (if positive?
             (setMotors speed+ speed-)
             (setMotors speed- speed+))
         (resetCount left-wheel)
         (bcom (^move-stopper bcom resolver lcd left-wheel distance (if positive? >= <=)) vow)]))]
   [(tick time)
    ($ lcd "^motors" 0)
    ($ lcd (~v (getCount right-wheel)) 4)]))

(define (^lcd bcom)
  setLCDMessage)

(define (^move-stopper bcom resolver lcd wheel final-distance comparison)
  (methods
   [(tick time)
    (let ([current-distance (getCount wheel)])
      ($ lcd "^move-stopper" 0)
      ($ lcd (~v current-distance) 2)
      (if (comparison current-distance final-distance)
          (begin
            (stopMotors)
            (<- resolver 'fulfill current-distance)
            (bcom (^motors bcom lcd)))
          #f))]))
