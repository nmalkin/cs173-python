#lang plai-typed

(define-type PyExpr
  [PySeq (es : (listof PyExpr))]
  [PyAssign (targets : (listof PyExpr)) (value : expr)]
  [PyNum (n : number)]
  [PyBool (b : boolean)]
  [PyStr (s : string)]
  [PyId (x : symbol)]
  [PyRaise (expr : PyExpr) (cause : PyExpr)]
  [PyPass]
  [PyIf (test : PyExpr) (body : PyExpr) (orelse : PyExpr)]
  [PyBinOp (left : PyExpr) (op : symbol) (right : PyExpr)]
  [PyUnaryOp (op : symbol) (operand : PyExpr)]
  [PyLam (args : (listof symbol)) (body : PyExpr)]
  [PyApp (fun : PyExpr) (args : (listof PyExpr))])

