#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt")

(define (desugar expr)
  (type-case PyExpr expr
    [PySeq (es) (foldl (lambda (e1 e2) (CSeq e2 (desugar e1))) (desugar (first es)) (rest es))]
    [PyNum (n) (CNum n)]
    [PyApp (f args) (CApp (desugar f) (map desugar args))]
    [PyId (x ctx) (CId x)]
    [else (error 'desugar "haven't implemented a case yet")]))
