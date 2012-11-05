#lang plai-typed

(require "../python-core-syntax.rkt" "../util.rkt")

(define (def (name : symbol) (expr : CExpr)) : CExpr
  (CAssign (CId name) expr))

(define int
  (CClass
    (list 'object)
    (seq-ops (list
               (def '__add__
                    (CFunc (list 'self 'other)
                           (CIf ; type-checking
                             (CBuiltinPrim 'str=
                                           (list
                                               (CGetField (CId 'self) '__class__)
                                               (CGetField (CId 'other) '__class__)))
                             (CBuiltinPrim 'num+
                                           (list
                                               (CId 'self)
                                               (CId 'other)))
                             (CError (CStr "unsupported operand type(s)")))))))
    ))






