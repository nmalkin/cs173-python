#lang plai-typed

(define-type PyExpr
  ; control structures
  [PyIf (test : PyExpr) (body : PyExpr) (orelse : PyExpr)]
  [PySeq (es : (listof PyExpr))]
  [PyAssign (targets : (listof PyExpr)) (value : PyExpr)]

  ; primitive literals
  [PyNum (n : number)]
  [PyBool (b : boolean)]
  [PyId (x : symbol) (ctx : symbol)]

  ;
  [PyRaise (expr : PyExpr)]
  [PyPass]

  ; operations
  [PyBinOp (left : PyExpr) (op : symbol) (right : PyExpr)] ;op = 'Add | 'Sub | etc
  [PyUnaryOp (op : symbol) (operand : PyExpr)]

  [PyCompOp (left : PyExpr) 
            (ops : (listof symbol)) ;ops = 'Eq | 'NotEq | 'Lt etc
            (comparators : (listof PyExpr))]
  [PyBoolOp (op : symbol) (values : (listof PyExpr))] ;op = 'And | 'Or

  ; functions
  [PyLam (args : (listof symbol)) (body : PyExpr)]
  [PyApp (fun : PyExpr) (args : (listof PyExpr))]

  ; builtin data structures
  [PyStr (s : string)]
  [PyDict (keys : (listof PyExpr)) (values : (listof PyExpr))])



