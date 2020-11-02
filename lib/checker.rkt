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

  (define (check-all-assigned ctx targets ent-name mode [inst-name #f])
    (define ports (meta/entity-ports (lookup ent-name)))
    (for ([p (in-dict-values ports)]
          #:when (eq? mode (meta/port-mode p)))
      (define port-key (make-port-key (meta/port-name p) inst-name))
      (unless (set-member? targets port-key)
        (raise-syntax-error (first port-key) "Port is never assigned" ctx))))

  (define assignment-targets (make-parameter #f))

  (define (check stx)
    (syntax-parse stx
      #:datum-literals [begin-tiny-hdl]

      [(begin-tiny-hdl body ...)
       #`(begin
           #,@(map check (attribute body)))]

      [:stx/architecture
       (parameterize ([assignment-targets (collect-assignment-targets (attribute body))])
         (check-all-assigned stx (assignment-targets) #'ent-name 'output)
         (map check (attribute body)))
       stx]

      [:stx/instance
       #:with ent-name (meta/architecture-ent-name (lookup #'arch-name))
       (check-all-assigned stx (assignment-targets) #'ent-name 'input #'name)
       stx]

      [_ stx])))
