#lang racket

(provide
  make-half-adder-arch make-full-adder-arch
  (struct-out half-adder)
  (struct-out full-adder))

(struct half-adder ([a  #:auto]
                    [b  #:auto]
                    [s  #:auto]
                    [co #:auto]) #:mutable)

(define (make-half-adder-arch)
  (define self (half-adder))
  (set-half-adder-s!  self (λ () (xor ((half-adder-a self)) ((half-adder-b self)))))
  (set-half-adder-co! self (λ () (and ((half-adder-a self)) ((half-adder-b self)))))
  self)

(struct full-adder ([a  #:auto]
                    [b  #:auto]
                    [ci #:auto]
                    [s  #:auto]
                    [co #:auto]) #:mutable)

(define (make-full-adder-arch)
  (define self (full-adder))
  (define h1   (make-half-adder-arch))
  (define h2   (make-half-adder-arch))
  (set-half-adder-a!  h1   (λ () ((full-adder-a  self))))
  (set-half-adder-b!  h1   (λ () ((full-adder-b  self))))
  (set-half-adder-a!  h2   (λ () ((half-adder-s  h1))))
  (set-half-adder-b!  h2   (λ () ((full-adder-ci self))))
  (set-full-adder-s!  self (λ () ((half-adder-s  h2))))
  (set-full-adder-co! self (λ () (or ((half-adder-co h1)) ((half-adder-co h2)))))
  self)
