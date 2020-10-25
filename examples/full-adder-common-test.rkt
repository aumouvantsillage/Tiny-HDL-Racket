
; Common test code.
; This file is meant to be included from one of the other test modules.

; Create an instance of a full adder.
(define fa (full-adder-arch))

; Print a row of the truth table of the full adder.
(define-syntax-rule (print-truth-table-row xa xb xci)
  (begin
    (set-full-adder-a!  fa (λ () xa))
    (set-full-adder-b!  fa (λ () xb))
    (set-full-adder-ci! fa (λ () xci))
    (printf "~a ~a ~a -> ~a ~a~n" xa xb xci
      ((full-adder-s  fa))
      ((full-adder-co fa)))))

; Print the truth table of the full adder.
(displayln " a  b ci     s co")
(print-truth-table-row #f #f #f)
(print-truth-table-row #f #f #t)
(print-truth-table-row #f #t #f)
(print-truth-table-row #f #t #t)
(print-truth-table-row #t #f #f)
(print-truth-table-row #t #f #t)
(print-truth-table-row #t #t #f)
(print-truth-table-row #t #t #t)
