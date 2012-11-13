#lang plai-typed 

(require "python-core-syntax.rkt")

(require
 (typed-in racket/base 
           (hash-for-each : ((hashof 'a 'b) ('c 'd -> 'e) -> void))))

; a file for utility functions that aren't specific to python stuff


; lists->hash - given two parallel list produce a mutable hash mapping 
; values from one to values in the other
(define (lists->hash [l1 : (listof 'a)] [l2 : (listof 'b)]) : (hashof 'a 'b)
  (local [(define h (make-hash empty))]
    (begin
      (map2 (lambda (k v) (hash-set! h k v))
            l1 l2)
      h)))

(test (lists->hash (list "a" "b" "c") (list 1 2 3))
      (let ([h (make-hash empty)])
        (begin
          (hash-set! h "a" 1)
          (hash-set! h "b" 2)
          (hash-set! h "c" 3) 
          h)))

(define (immutable-hash-copy h)
  (let ([r (hash empty)])
    (begin
      (hash-for-each h (lambda (k v) (set! r (hash-set r k v))))
      r)))

(define (seq-ops (ops : (listof CExpr))) : CExpr
  (cond 
    [(= 1 (length ops)) (first ops)]
    [else (CSeq (first ops) 
                (seq-ops (rest ops)))]))

(define (def (name : symbol) (expr : CExpr)) : CExpr
  (CAssign (CId name) expr))

(define-syntax (check-types x)
  (syntax-case x ()
    [(check-types args t1 body)
     (with-syntax ([mval1 (datum->syntax x 'mval1)])
       #'(let ([arg1 (first args)])
           (if (and (VObject? arg1) (symbol=? (VObject-antecedent arg1) t1))
               (let ([mayb-mval1 (VObject-mval arg1)])
                 (if (some? mayb-mval1)
                     (let ([mval1 (some-v mayb-mval1)])
                       body)
                     (none)))
               (none))))]
    [(check-types args t1 t2 body)
     (with-syntax ([mval1 (datum->syntax x 'mval1)]
                   [mval2 (datum->syntax x 'mval2)])
       #'(let ([arg1 (first args)]
               [arg2 (second args)])
           (if (and (VObject? arg1) (VObject? arg2)
                    (symbol=? (VObject-antecedent arg1) t1)
                    (symbol=? (VObject-antecedent arg2) t2))
               (let ([mayb-mval1 (VObject-mval arg1)]
                     [mayb-mval2 (VObject-mval arg2)])
                 (if (and (some? mayb-mval1) (some? mayb-mval2))
                     (let ([mval1 (some-v mayb-mval1)]
                           [mval2 (some-v mayb-mval2)])
                       body)
                     (none)))
               (none))))]))
