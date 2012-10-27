#lang plai-typed

(define-type PyExpr
  [PySeq (es : (listof PyExpr))]
  [PyAssign (targets : (listof PyExpr)) (value : PyExpr)]
  [PyNum (n : number)]
  [PyBool (b : boolean)]
  [PyStr (s : string)]
  [PyId (x : symbol) (ctx : symbol)]
  [PyRaise (expr : PyExpr)]
  [PyPass]
  [PyIf (test : PyExpr) (body : PyExpr) (orelse : PyExpr)]
  [PyBinOp (left : PyExpr) (op : symbol) (right : PyExpr)] ;op = 'Add | 'Sub | etc
  [PyUnaryOp (op : symbol) (operand : PyExpr)]

  [PyCompOp (left : PyExpr) 
            (ops : (listof symbol)) ;ops = 'Eq | 'NotEq | 'Lt etc
            (comparators : (listof PyExpr))]
  [PyLam (args : (listof symbol)) (body : PyExpr)]
  [PyApp (fun : PyExpr) (args : (listof PyExpr))])

