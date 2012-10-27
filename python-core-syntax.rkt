#lang plai-typed

#|

This is the core language; it is just borrowing a few things from 
ParselTongue.

|#

(require (typed-in racket/base (number->string : (number -> string))))


(define-type CExpr
  [CNum (n : number)]
  [CStr (s : string)]
  [CTrue]
  [CFalse]
  [CSeq (e1 : CExpr) (e2 : CExpr)]
  [CAssign (targets : (listof CExpr)) (value : CExpr)]
  [CError (e1 : CExpr)]
  [CIf (test : CExpr) (then : CExpr) (else : CExpr)]
  [CId (x : symbol)]
  [CLet (x : symbol) (bind : CExpr) (body : CExpr)]
  [CApp (fun : CExpr) (args : (listof CExpr))]
  [CFunc (args : (listof symbol)) (body : CExpr)]
  [CPrim1 (prim : symbol) (arg : CExpr)]
  [CPrim2 (prim : symbol) (arg1 : CExpr) (arg2 : CExpr)])

(define-type CVal
  [VNum (n : number)]
  [VStr (s : string)]
  [VTrue]
  [VFalse]
  [VClosure (env : Env) (args : (listof symbol)) (body : CExpr)])

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
  [v*s (v : CVal) (s : Store)])