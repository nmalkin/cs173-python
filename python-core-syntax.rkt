#lang plai-typed

#|

This is the core language; it is just borrowing a few things from 
ParselTongue.

|#

(require (typed-in racket/base (number->string : (number -> string))))


(define-type CExpr
  [CStr (s : string)]
  [CTrue]
  [CFalse]
  [CNone]
  [CClass (name : symbol) (base : symbol) (body : CExpr)]
  [CObject (class : symbol) (bval : (optionof MetaVal))]
  [CGetField (value : CExpr) (attr : symbol)]
  [CSeq (e1 : CExpr) (e2 : CExpr)]
  [CAssign (target : CExpr) (value : CExpr)]
  [CError (e1 : CExpr)]
  [CIf (test : CExpr) (then : CExpr) (else : CExpr)]
  [CId (x : symbol)]
  [CLet (x : symbol) (bind : CExpr) (body : CExpr)]
  [CApp (fun : CExpr) (args : (listof CExpr))]
  [CFunc (args : (listof symbol)) (body : CExpr)]
  [CReturn (value : CExpr)]
  [CPrim1 (prim : symbol) (arg : CExpr)]
  [CPrim2 (prim : symbol) (arg1 : CExpr) (arg2 : CExpr)]
  [CBuiltinPrim (op : symbol) (args : (listof CExpr))]

  [CList (values : (listof CExpr))]
  [CDict (contents : (hashof CExpr CExpr))])

(define-type CVal
  [VStr (s : string)]
  [VTrue]
  [VFalse]
  [VNone]
  [VObject (antecedent : symbol) (mval : (optionof MetaVal)) (dict : object-dict)]
  [VClosure (env : Env) (args : (listof symbol)) (body : CExpr)]
  [VDict (contents : (hashof CVal CVal))])

(define-type MetaVal
             [MetaNum (n : number)]
						 [MetaStr (s : string)]
             [MetaClass]
             [MetaList (l : (listof CVal))])
             

;; env is a listof hashof's so there are deliniations between closures
(define-type-alias Env (listof (hashof symbol Address)))

(define-type-alias Address number)
(define Address->string number->string)
(define-type-alias Store (hashof Address CVal))
(define new-loc
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))

(define-type Result
  [v*s*e (v : CVal) (s : Store) (e : Env)]
  [Return (v : CVal) (s : Store) (e : Env)])

(define-type-alias object-dict (hashof symbol Address))
