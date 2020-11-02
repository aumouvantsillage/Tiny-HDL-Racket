#lang racket

(require
  (prefix-in stx/ "syntax.rkt")
  (for-syntax
    racket
    syntax/parse
    "scope.rkt"
    (prefix-in meta/ "meta.rkt")))

(provide
  (for-syntax check))

(begin-for-syntax
  (define (make-port-key port-name inst-name)
    (list
      (syntax->datum port-name)
      (and inst-name (syntax->datum inst-name))))

  (define (collect-assignment-targets stmt-lst)
    (for/fold ([acc  (set)])
              ([stmt (in-list stmt-lst)])
      (syntax-parse stmt
        [:stx/assignment
         #:with (_ ent-name port-name (~optional inst-name)) #'target
         (define port-key (make-port-key #'port-name (attribute inst-name)))

         ; Check that the port has the right mode.
         (define port (dict-ref (meta/entity-ports (lookup #'ent-name)) #'port-name))
         (define expected-mode (if (attribute inst-name) 'input 'output))
         (unless (eq? expected-mode (meta/port-mode port))
           (raise-syntax-error (first port-key) "Invalid target for assignment" #'target))

         ; Check that the port is assigned only once in the current architecture.
         (when (set-member? acc port-key)
           (raise-syntax-error (first port-key) "Port is assigned more than one time" #'target))
           
         (set-add acc port-key)]

        [_ acc])))

  (define (check stx)
    (syntax-parse stx
      #:datum-literals [begin-tiny-hdl]

      [(begin-tiny-hdl body ...)
       #`(begin
           #,@(map check (attribute body)))]

      [:stx/architecture
       (collect-assignment-targets (attribute body))
       stx]

      [_ stx])))
