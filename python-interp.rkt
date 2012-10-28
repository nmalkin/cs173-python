#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt"
         (typed-in racket/base (hash-copy : ((hashof 'a 'b) -> (hashof 'a 'b))))
         (typed-in racket/base (expt : (number number -> number)))
         (typed-in racket/base (quotient : (number number -> number))))

;; interp-env : CExpr * Env * Store -> Result
(define (interp-env [expr : CExpr] [env : Env] [sto : Store]) : Result
  (type-case CExpr expr
    [CNum (n) (v*s (VNum n) sto)]
    [CStr (s) (v*s (VStr s) sto)]
    [CTrue () (v*s (VTrue) sto)]
    [CFalse () (v*s (VFalse) sto)]
    [CNone () (v*s (VNone) sto)]

    [CSeq (e1 e2) (type-case Result (interp-env e1 env sto)
                    [v*s (v1 s1) (interp-env e2 env s1)])]
    
    ;; deal with pythonic scope here
    ;; only for ids!
    [CAssign (t v) (type-case Result (interp-env v env sto)
                       [v*s (vv sv) (type-case (optionof Address) (hash-ref (first env) (CId-x t))
                                      [some (w) (begin
                                                  (cons (hash-set (first env) (CId-x t) w) (rest env))
                                                  (hash-set sto w vv)
                                                  (v*s (VNone) sv))]
                                      [none () (let ([w (new-loc)])
                                                 (begin
                                                   (cons (hash-set (hash-copy (first env)) (CId-x t) w) (rest env))
                                                   (hash-set sto w vv)
                                                   (v*s (VNone) sv)))])])]
                                                     
    
    [CError (e) (error 'interp (to-string (interp-env e env sto)))]

    [CIf (i t e) (type-case Result (interp-env i env sto)
                   [v*s (vi si) (type-case CVal vi
                                  [VTrue () (interp-env t env si)]
                                  [else (interp-env e env si)])])]

    [CId (x) (type-case (optionof Address) (hash-ref (first env) x)
      [some (v) (type-case (optionof CVal) (hash-ref sto v)
                  [some (v2) (v*s v2 sto)]
                  [none () (error 'interp (string-append "No value at address " (Address->string v)))])]
      [none () (error 'interp "Unbound identifier")])]

    [CLet (x bind body)
          (let ([w (new-loc)])
            (interp-env body
                        (cons (hash-set (first env) x w) (rest env))
                        (hash-set sto w (v*s-v (interp-env bind env sto)))))]

    [CApp (fun arges)
     (type-case Result (interp-env fun env sto)
       [v*s (vfun sfun) (type-case CVal vfun
                          [VClosure (cenv argxs body)
                                    (let ([sa sto])
                                      (local [(define argvs 
                                                (map (lambda (e) 
                                                       (type-case Result (interp-env e cenv sa)
                                                         [v*s (varg sarg) 
                                                              (begin 
                                                                (set! sa sarg)
                                                                varg)])) arges))]
                                        (interp-env body (bind-args argxs argvs env sa) sa)))]
                          [else (error 'interp "Not a closure")])])]

    ;; lambdas for now, implement real functions later
    [CFunc (args body) (v*s (VClosure (cons (hash empty) env) args body) sto)]

    [CPrim1 (prim arg) (type-case Result (interp-env arg env sto)
                         [v*s (varg sarg) (v*s (python-prim1 prim varg) sarg)])]
    
    ;; implement this
    [CPrim2 (prim arg1 arg2) 
            (type-case Result (interp-env arg1 env sto)
              [v*s (varg1 sarg1)
                   (type-case Result (interp-env arg2 env sarg1)
                     [v*s (varg2 sarg2) 
                          (case prim
                            ['Add (cond 
                                    [(and (VNum? varg1) (VNum? varg2)) (v*s (VNum (+ (VNum-n varg1) (VNum-n varg2))) sarg2)]
                                    [(and (VStr? varg1) (VStr? varg2)) (v*s (VStr (string-append (VStr-s varg1) (VStr-s varg2))) sarg2)]
                                    [else (error 'interp "Bad arguments to +")])]
                            ['Sub (cond
                                    [(and (VNum? varg1) (VNum? varg2)) (v*s (VNum (- (VNum-n varg1) (VNum-n varg2))) sarg2)]
                                    [else (error 'interp "Bad arguments to -")])]
                            ['Mult (cond
                                     [(and (VNum? varg1) (VNum? varg2)) (v*s (VNum (* (VNum-n varg1) (VNum-n varg2))) sarg2)]
                                     [else (error 'interp "Bad arguments to *")])]
                            ;; need to make sure division is implemented correctly
                            ['Div (cond
                                     [(and (VNum? varg1) (VNum? varg2) (> 0 (VNum-n varg2))) (v*s (VNum (/ (VNum-n varg1) (VNum-n varg2))) sarg2)]
                                     [else (error 'interp "Bad arguments to *")])]
                            ['Mod (cond
                                    [(and (VNum? varg1) (VNum? varg2) (> 0 (VNum-n varg2))) (v*s (VNum (modulo (VNum-n varg1) (VNum-n varg2))) sarg2)]
                                    [else (error 'interp "Bad arguments to %")])]
                            ['Pow (cond
                                    [(and (VNum? varg1) (VNum? varg2)) (v*s (VNum (expt (VNum-n varg1) (VNum-n varg2))) sarg2)]
                                    [else (error 'interp "Bad arguments to **")])]
                            ['FloorDiv (cond
                                         [(and (VNum? varg1) (VNum? varg2) (> 0 (VNum-n varg2))) (v*s (VNum (quotient (VNum-n varg1) (VNum-n varg2))) sarg2)]
                                         [else (error 'interp "Bad arguments to //")])]
                            ['Lt (cond
                                  [(and (VNum? varg1) (VNum? varg2)) (v*s (if (< (VNum-n varg1) (VNum-n varg2))
                                                                              (VTrue)
                                                                              (VFalse)) sarg2)]
                                  [else (error 'interp "Bad arguments to <")])]
                            ['Gt (cond
                                  [(and (VNum? varg1) (VNum? varg2)) (v*s (if (> (VNum-n varg1) (VNum-n varg2))
                                                                              (VTrue)
                                                                              (VFalse)) sarg2)]
                                  [else (error 'interp "Bad arguments to >")])]
                            ['LtE (cond
                                  [(and (VNum? varg1) (VNum? varg2)) (v*s (if (<= (VNum-n varg1) (VNum-n varg2))
                                                                              (VTrue)
                                                                              (VFalse)) sarg2)]
                                  [else (error 'interp "Bad arguments to <=")])]
                            ['GtE (cond
                                  [(and (VNum? varg1) (VNum? varg2)) (v*s (if (>= (VNum-n varg1) (VNum-n varg2))
                                                                              (VTrue)
                                                                              (VFalse)) sarg2)]
                                  [else (error 'interp "Bad arguments to >=")])]
                            ['Eq (cond
                                  [(and (VNum? varg1) (VNum? varg2)) (v*s (if (eq? (VNum-n varg1) (VNum-n varg2))
                                                                              (VTrue)
                                                                              (VFalse)) sto)]
                                  [else (error 'interp "Bad arguments to ==")])]
                            ['NotEq (cond
                                  [(and (VNum? varg1) (VNum? varg2)) (v*s (if (not (eq? (VNum-n varg1) (VNum-n varg2)))
                                                                              (VTrue)
                                                                              (VFalse)) sarg2)]
                                  [else (error 'interp "Bad arguments to !=")])]
                            ;; Handle Is, IsNot, In, NotIn
                            ['Is (cond
                                  [(and (VNum? varg1) (VNum? varg2)) (v*s (if (not (eq? (VNum-n varg1) (VNum-n varg2)))
                                                                              (VTrue)
                                                                              (VFalse)) sarg2)]
                                  [else (error 'interp "Bad arguments to !=")])]
                            )])])]
    
    ;[else (error 'interp "haven't implemented a case yet")]
    ))

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
  (type-case Result (interp-env expr (list (hash (list))) (hash (list)))
    [v*s (vexpr sexpr) vexpr]))
