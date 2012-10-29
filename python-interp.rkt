#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt"
         (typed-in racket/base (hash-copy : ((hashof 'a 'b) -> (hashof 'a 'b))))
         (typed-in racket/base (expt : (number number -> number)))
         (typed-in racket/base (quotient : (number number -> number)))
         (typed-in racket/base (string<? : (string string -> boolean)))
         (typed-in racket/base (string>? : (string string -> boolean)))
         (typed-in racket/base (string<=? : (string string -> boolean)))
         (typed-in racket/base (string>=? : (string string -> boolean)))
         (typed-in racket/base (hash-for-each : ((hashof 'a 'b) ('c 'd -> 'e) -> void))))

;; interp-env : CExpr * Env * Store -> Result
(define (interp-env [expr : CExpr] [env : Env] [sto : Store]) : Result
  (type-case CExpr expr
    [CNum (n) (v*s*e (VNum n) sto env)]
    [CStr (s) (v*s*e (VStr s) sto env)]
    [CTrue () (v*s*e (VTrue) sto env)]
    [CFalse () (v*s*e (VFalse) sto env)]
    [CNone () (v*s*e (VNone) sto env)]

    [CSeq (e1 e2) (type-case Result (interp-env e1 env sto)
                    [v*s*e (v1 s1 new-env) (interp-env e2 new-env s1)])]
    
    ;; deal with pythonic scope here
    ;; only for ids!
    [CAssign (t v) 
             (type-case Result (interp-env v env sto)
                       [v*s*e (vv sv venv) (type-case (optionof Address) (hash-ref (first env) (CId-x t))
                                      [some (w) (begin
                                                  
                                                  (v*s*e (VNone)
                                                         (hash-set sto w vv) 
                                                         (cons (hash-set (first venv) (CId-x t) w) (rest venv))))]
                                      [none () (let ([w (new-loc)])
                                                 (begin
                                                   (v*s*e (VNone)
                                                          (hash-set sto w vv)
                                                          (cons (hash-set (immutable-hash-copy (first env)) (CId-x t) w) (rest env)))))])])]
                                                     
    
    [CError (e) (error 'interp (to-string (interp-env e env sto)))]

    [CIf (i t e) (type-case Result (interp-env i env sto)
                   [v*s*e (vi si envi) (type-case CVal (truthy? vi)
                                     [VTrue () (interp-env t envi si)]
                                     [else (interp-env e envi si)])])]

    [CId (x) (fetch (lookup x env) sto env)]

    [CLet (x bind body)
          (let ([w (new-loc)])
            (interp-env body
                        (cons (hash-set (first env) x w) (rest env))
                        (hash-set sto w (v*s*e-v (interp-env bind env sto)))))]

    [CApp (fun arges)
     (type-case Result (interp-env fun env sto)
       [v*s*e (vfun sfun efun) (type-case CVal vfun
                          [VClosure (cenv argxs body)
                                    (let ([sa sto])
                                      (local [(define argvs 
                                                (map (lambda (e) 
                                                       (type-case Result (interp-env e cenv sa)
                                                         [v*s*e (varg sarg envarg) 
                                                              (begin 
                                                                (set! sa sarg)
                                                                varg)])) arges))]
                                        (local [(define-values (e s) (bind-args argxs argvs env sa))]
                                          (interp-env body e s))))]
                          [else (error 'interp "Not a closure")])])]

    ;; lambdas for now, implement real functions later
    [CFunc (args body) (v*s*e (VClosure (cons (hash empty) env) args body) sto env)]

    [CPrim1 (prim arg)
            (type-case Result (interp-env arg env sto)
              [v*s*e (varg sarg envarg) 
                   (case prim
                     ['Not (type-case CVal (truthy? varg)
                             [VTrue () (v*s*e (VFalse) sarg envarg)]
                             [else (v*s*e (VTrue) sarg envarg)])]
                     ['USub (interp-env (CPrim2 'Sub (CNum 0) arg) env sto)]
                     ['UAdd (interp-env (CPrim2 'Add (CNum 0) arg) env sto)]
                     [else (v*s*e (python-prim1 prim varg) sarg envarg)])])]
    
    ;; implement this
    [CPrim2 (prim arg1 arg2) 
            (type-case Result (interp-env arg1 env sto)
              [v*s*e (varg1 sarg1 envarg1)
                   (type-case Result (interp-env arg2 envarg1 sarg1)
                     [v*s*e (varg2 sarg2 envarg2) 
                          (case prim
                            ['Add (cond 
                                    [(and (VNum? varg1) (VNum? varg2)) (v*s*e (VNum (+ (VNum-n varg1) (VNum-n varg2))) sarg2 envarg2)]
                                    [(and (VStr? varg1) (VStr? varg2)) (v*s*e (VStr (string-append (VStr-s varg1) (VStr-s varg2))) sarg2 envarg2)]
                                    [else (error 'interp "Bad arguments to +")])]
                            ['Sub (cond
                                    [(and (VNum? varg1) (VNum? varg2)) (v*s*e (VNum (- (VNum-n varg1) (VNum-n varg2))) sarg2 envarg2)]
                                    [else (error 'interp "Bad arguments to -")])]
                            ['Mult (cond
                                     [(and (VNum? varg1) (VNum? varg2)) (v*s*e (VNum (* (VNum-n varg1) (VNum-n varg2))) sarg2 envarg2)]
                                     [else (error 'interp "Bad arguments to *")])]
                            ;; need to make sure division is implemented correctly
                            ['Div (cond
                                     [(and (VNum? varg1) (VNum? varg2) (< 0 (VNum-n varg2))) (v*s*e (VNum (/ (VNum-n varg1) (VNum-n varg2))) sarg2 envarg2)]
                                     [else (error 'interp "Bad arguments to /")])]
                            ['Mod (cond
                                    [(and (VNum? varg1) (VNum? varg2) (< 0 (VNum-n varg2))) (v*s*e (VNum (modulo (VNum-n varg1) (VNum-n varg2))) sarg2 envarg2)]
                                    [else (error 'interp "Bad arguments to %")])]
                            ['Pow (cond
                                    [(and (VNum? varg1) (VNum? varg2)) (v*s*e (VNum (expt (VNum-n varg1) (VNum-n varg2))) sarg2 envarg2)]
                                    [else (error 'interp "Bad arguments to **")])]
                            ['FloorDiv (cond
                                         [(and (VNum? varg1) (VNum? varg2) (< 0 (VNum-n varg2))) (v*s*e (VNum (quotient (VNum-n varg1) (VNum-n varg2))) sarg2 envarg2)]
                                         [else (error 'interp "Bad arguments to //")])]
                            ['Lt (cond
                                  [(and (VNum? varg1) (VNum? varg2)) (v*s*e (if (< (VNum-n varg1) (VNum-n varg2))
                                                                              (VTrue)
                                                                              (VFalse)) sarg2 envarg2)]
                                  [(and (VStr? varg1) (VStr? varg2)) (v*s*e (if (string<? (VStr-s varg1) (VStr-s varg2))
                                                                              (VTrue)
                                                                              (VFalse)) sarg2 envarg2)]
                                  [else (error 'interp "Bad arguments to <")])]
                            ['Gt (cond
                                  [(and (VNum? varg1) (VNum? varg2)) (v*s*e (if (> (VNum-n varg1) (VNum-n varg2))
                                                                              (VTrue)
                                                                              (VFalse)) sarg2 envarg2)]
                                  [(and (VStr? varg1) (VStr? varg2)) (v*s*e (if (string>? (VStr-s varg1) (VStr-s varg2))
                                                                              (VTrue)
                                                                              (VFalse)) sarg2 envarg2)]
                                  [else (error 'interp "Bad arguments to >")])]
                            ['LtE (cond
                                  [(and (VNum? varg1) (VNum? varg2)) (v*s*e (if (<= (VNum-n varg1) (VNum-n varg2))
                                                                              (VTrue)
                                                                              (VFalse)) sarg2 envarg2)]
                                  [(and (VStr? varg1) (VStr? varg2)) (v*s*e (if (string<=? (VStr-s varg1) (VStr-s varg2))
                                                                              (VTrue)
                                                                              (VFalse)) sarg2 envarg2)]
                                  [else (error 'interp "Bad arguments to <=")])]
                            ['GtE (cond
                                  [(and (VNum? varg1) (VNum? varg2)) (v*s*e (if (>= (VNum-n varg1) (VNum-n varg2))
                                                                              (VTrue)
                                                                              (VFalse)) sarg2 envarg2)]
                                  [(and (VStr? varg1) (VStr? varg2)) (v*s*e (if (string>=? (VStr-s varg1) (VStr-s varg2))
                                                                              (VTrue)
                                                                              (VFalse)) sarg2 envarg2)]
                                  [else (error 'interp "Bad arguments to >=")])]
                            ['Eq (cond
                                  [(and (VNum? varg1) (VNum? varg2)) (v*s*e (if (= (VNum-n varg1) (VNum-n varg2))
                                                                              (VTrue)
                                                                              (VFalse)) sarg2 envarg2)]
                                  [(and (VStr? varg1) (VStr? varg2)) (v*s*e (if (string=? (VStr-s varg1) (VStr-s varg2))
                                                                              (VTrue)
                                                                              (VFalse)) sarg2 envarg2)]
                                  [else (error 'interp "Bad arguments to ==")])]
                            ['NotEq (cond
                                  [(and (VNum? varg1) (VNum? varg2)) (v*s*e (if (not (= (VNum-n varg1) (VNum-n varg2)))
                                                                              (VTrue)
                                                                              (VFalse)) sarg2 envarg2)]
                                  [(and (VStr? varg1) (VStr? varg2)) (v*s*e (if (not (string=? (VStr-s varg1) (VStr-s varg2)))
                                                                              (VTrue)
                                                                              (VFalse)) sarg2 envarg2)]
                                  [else (error 'interp "Bad arguments to !=")])]
                            ;; Handle Is, IsNot, In, NotIn
                            [else (error 'interp (string-append "Haven't implemented a case yet: " (symbol->string prim)))]
                            )])])]
    
    ;[else (error 'interp "haven't implemented a case yet")]
    ))

(define (lookup x env)
  (cond
    [(empty? env) (error 'interp (string-append "Unbound identifier: " (symbol->string x)))]
    [else (type-case (optionof Address) (hash-ref (first env) x)
            [some (v) v]
            [none () (lookup x (rest env))])]))

(define (fetch w sto env)
  (type-case (optionof CVal) (hash-ref sto w)
    [some (v) (v*s*e v sto env)]
    [none () (error 'interp (string-append "No value at address " (Address->string w)))]))

(define (immutable-hash-copy h)
  (let ([r (hash empty)])
    (begin
      (hash-for-each h (lambda (k v) (set! r (hash-set r k v))))
      r)))

(define (bind-args args vals [env : Env] [sto : Store])
  (cond [(and (empty? args) (empty? vals)) (values env sto)]
        [(or (empty? args) (empty? vals))
         (error 'interp "Arity mismatch")]
        [(and (cons? args) (cons? vals))
         (let ([where (new-loc)])
               (begin
                 (let ([e (cons (hash-set (first env) (first args) where) (rest env))]
                       [s (hash-set sto where (first vals))])
                   (bind-args (rest args) (rest vals) e s))))]))

(define (truthy? val)
  (type-case CVal val
    [VNum (n) (if (= 0 n)
              (VFalse)
              (VTrue))]
    [VStr (s) (if (string=? "" s)
              (VFalse)
              (VTrue))]
    [VTrue () val]
    [VFalse () val]
    [VNone () (VFalse)]
    [VClosure (e a b) (VTrue)]))

(define (interp expr)
  (type-case Result (interp-env expr (list (hash (list))) (hash (list)))
    [v*s*e (vexpr sexpr env) (if (not (VNone? vexpr)) 
                         (begin (display (pretty vexpr)) 
                                (display "\n"))
                         (display ""))]))
