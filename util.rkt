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
  (foldl
    (lambda (next sofar)
      (CSeq next sofar))
    (first ops)
    (rest ops)))
