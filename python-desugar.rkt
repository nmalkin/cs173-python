#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt")
(define (desugar (expr : PyExpr)) : CExpr
  (type-case PyExpr expr
    [PySeq (es) (foldl (lambda (e1 e2) (CSeq e2 (desugar e1))) (desugar (first es)) (rest es))]
    ;[PyAssign (t v) (CAssign (map desugar t) (desugar v))]
    [PyNum (n) (CNum n)]
    [PyBool (b) (if b (CTrue) (CFalse))]
    [PyStr (s) (CStr s)]
    [PyId (x ctx) (CId x)]
    [PyRaise (expr) (CError (desugar expr))]
    [PyPass () ; no-op function implemented as a lambda returning dummy value (None)
            (CFunc empty (CNone))]
    [PyIf (test body orelse)
          (CIf (desugar test) (desugar body) (desugar orelse))]
    [PyBinOp (left op right) (CPrim2 op (desugar left) (desugar right))]
    [PyUnaryOp (op operand) (CPrim1 op (desugar operand))]
    [PyApp (f args) (CApp (desugar f) (map desugar args))]
    [else (error 'desugar "haven't implemented a case yet")]))
