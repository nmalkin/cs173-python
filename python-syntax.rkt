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
  [PyClass (name : symbol) (bases : (listof symbol)) (body : PyExpr)]
  [PyDotField (value : PyExpr) (attr : symbol)]
  [PyIf (test : PyExpr) (body : PyExpr) (orelse : PyExpr)]
  [PyBinOp (left : PyExpr) (op : symbol) (right : PyExpr)] ;op = 'Add | 'Sub | etc
  [PyUnaryOp (op : symbol) (operand : PyExpr)]

  [PyCompOp (left : PyExpr) 
            (ops : (listof symbol)) ;ops = 'Eq | 'NotEq | 'Lt etc
            (comparators : (listof PyExpr))]
  [PyBoolOp (op : symbol) (values : (listof PyExpr))] ;op = 'And | 'Or
  [PyLam (args : (listof symbol)) (body : PyExpr)]
  [PyFunc (name : symbol) (args : (listof symbol)) (body : PyExpr)]
  [PyReturn (value : PyExpr)]
  [PyApp (fun : PyExpr) (args : (listof PyExpr))])

