#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt")

;; interp-env : CExpr * Env * Store -> CVal
(define (interp-env [expr : CExpr] [env : Env] [sto : Store]) : CVal
  (type-case CExpr expr
    [CNum (n) (VNum n)]
    [CStr (s) (VStr s)]
    [CTrue () (VTrue)]
    [CFalse () (VFalse)]

    [CError (e) (error 'interp (to-string (interp-env e env sto)))]

    [CIf (i t e) (type-case CVal (interp-env i env sto)
      [VTrue () (interp-env t env sto)]
      [else (interp-env e env sto)])]

    [CId (x) (type-case (optionof Address) (hash-ref (first env) x)
      [some (v) (type-case (optionof CVal) (hash-ref sto v)
                  [some (v2) v2]
                  [none () (error 'interp (string-append "No value at address " (Address->string v)))])]
      [none () (error 'interp "Unbound identifier")])]

    ;; need to deal with pythonic scope here
    ;; broken, just here so it will type-check
    [CLet (x bind body)
          (let ([where (new-loc)])
                (interp-env body 
                            (list (hash-set (first env) x where))
                            (hash-set sto where (interp-env bind env sto))))]

    [CSeq (e1 e2)
      (begin (interp-env e1 env sto) (interp-env e2 env sto))]

    [CApp (fun arges)
     (type-case CVal (interp-env fun env sto)
       [VClosure (cenv argxs body)
         (local [(define argvs (map (lambda (e) (interp-env e cenv sto)) arges))]
          (interp-env body (bind-args argxs argvs env sto) sto))]
       [else (error 'interp "Not a closure")])]

    [CFunc (args body) (VClosure env args body)]

    [CPrim1 (prim arg) (python-prim1 prim (interp-env arg env sto))]
    
    [else (error 'interp "haven't implemented a case yet")]))

(define (bind-args args vals [env : Env] [sto : Store])
  (cond [(and (empty? args) (empty? vals)) env]
        [(or (empty? args) (empty? vals))
         (error 'interp "Arity mismatch")]
        [(and (cons? args) (cons? vals))
         (let ([where (new-loc)])
               (begin
                 (hash-set (first env) (first args) where)
                 (hash-set sto where (first vals))
                 (bind-args (rest args) (rest vals) env sto)
                 env))]))

(define (interp expr)
  (interp-env expr (list (hash (list))) (hash (list))))

