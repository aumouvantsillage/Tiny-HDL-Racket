#lang racket

(provide
  half-adder-arch full-adder-arch
  (struct-out half-adder)
  (struct-out full-adder))

(struct half-adder ([a  #:auto]
                    [b  #:auto]
                    [s  #:auto]
                    [co #:auto]) #:mutable)

(define (half-adder-arch)
  (define io (half-adder))
  (set-half-adder-s!  io (λ () (xor ((half-adder-a io)) ((half-adder-b io)))))
  (set-half-adder-co! io (λ () (and ((half-adder-a io)) ((half-adder-b io)))))
  io)

(struct full-adder ([a  #:auto]
                    [b  #:auto]
                    [ci #:auto]
                    [s  #:auto]
                    [co #:auto]) #:mutable)

(define (full-adder-arch)
  (define io (full-adder))
  (define h1 (half-adder-arch))
  (define h2 (half-adder-arch))
  (set-half-adder-a!  h1 (λ () ((full-adder-a  io))))
  (set-half-adder-b!  h1 (λ () ((full-adder-b  io))))
  (set-half-adder-a!  h2 (λ () ((half-adder-s  h1))))
  (set-half-adder-b!  h2 (λ () ((full-adder-ci io))))
  (set-full-adder-s!  io (λ () ((half-adder-s  h2))))
  (set-full-adder-co! io (λ () (or ((half-adder-co h1)) ((half-adder-co h2)))))
  io)
