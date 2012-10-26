#lang plai-typed
(define-type ExprContext
   [Load]
   [Store]
   [Del]
   [AugLoad]
   [AugStore]
   [Param])

(define-type PyExpr
  [PySeq (es : (listof PyExpr))]
  [PyAssign (targets : (listof PyExpr)) (value : PyExpr)]
  [PyNum (n : number)]
  [PyBool (b : boolean)]
  [PyStr (s : string)]
  [PyId (x : symbol) (ctx : ExprContext)]
  [PyRaise (expr : PyExpr)]
  [PyPass]
  [PyIf (test : PyExpr) (body : PyExpr) (orelse : PyExpr)]
  [PyBinOp (left : PyExpr) (op : symbol) (right : PyExpr)]
  [PyUnaryOp (op : symbol) (operand : PyExpr)]
  [PyLam (args : (listof symbol)) (body : PyExpr)]
  [PyApp (fun : PyExpr) (args : (listof PyExpr))])

