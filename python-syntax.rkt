#lang plai-typed

(define-type PyExpr
  [PySeq (es : (listof PyExpr))]
  [PyNum (n : number)]
  [PyBool (b : boolean)]
  [PyStr (s : string)]
  [PyId (x : symbol)]
  [PyRaise (expr : PyExr) (cause : PyExpr)]
  [PyPass ()]
  [PyIf (test : PyExr) (body : PyExr) (orelse : PyExpr)]
  [PyBinOp (left : PyExpr) (op : symbol) (right : PyExpr)]
  [PyUnaryOp (op : symbol) (operand : PyExpr)]
  [PyLam (args : (listof symbol)) (body : PyExpr)]
  [PyApp (fun : PyExpr) (args : (listof PyExpr))])

