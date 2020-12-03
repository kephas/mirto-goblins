#lang racket

(require rebellion/type/enum)
(require goblins)
(require goblins/actor-lib/methods)
(require "MirtoEmulatorGui.rkt")



;;;;;;;;
; Start loop
;;;;;;;;

(define right-wheel 0)
(define left-wheel 1)
(define motor-1° 833)
(define counter-interval 1)

(define (forward/speed speed)
  (setMotors speed speed))

(define (rotate/speed speed)
  (setMotors speed (- 0 speed)))

  
;;;;;;;;;;;;;;;;
;;;; Actors ;;;;
;;;;;;;;;;;;;;;;

;; low level

(define (^motors bcom)
  (methods
   [(stop)
    (stopMotors)]
   [(set left right)
    (setMotors left right)]
   [(start-forward distance speed)
    (forward/speed speed)
    (enableCounters counter-interval)
    (resetCount right-wheel)
    (match (spawn-promise-cons)
      [(cons vow resolver)
       (setLCDMessage "wait for it" 0)
       (bcom (^move-stopper bcom resolver right-wheel distance >=))
       (setLCDMessage "wait for it..." 0)
       vow])]
   [(tick time)
    (setLCDMessage (~v (getCount right-wheel)) 2)]))

(define (^move-stopper bcom resolver wheel final-distance comparison)
  (methods
   [(tick time)
    (let ([current-distance (getCount wheel)])
      (setLCDMessage "." 0)
      (if (comparison current-distance final-distance)
          (begin
            (stopMotors)
            (bcom (^motors bcom))
            (<- resolver 'fulfill current-distance))
          #f))]))

;; high level

(define (^robot bcom motors)
  (lambda ()
    (on (<- motors 'start-forward 100000 200)
        (lambda (distance)
          (sleep 1)
          (<- motors 'start-forward 100000 200)))))




(define (start-loop time-step)
  (define control-vat (make-vat))
  (define plan-vat (make-vat))
  (open-asip)
  (define motors (control-vat 'spawn ^motors))
  (define robot (plan-vat 'spawn ^robot motors))
  (plan-vat 'run (lambda () (<- robot)))
  (let loop ()
    (control-vat 'run (lambda ()
                        (<- motors 'tick (current-milliseconds))))
    (sleep time-step)
    (loop)))
  



(start-loop .1)



