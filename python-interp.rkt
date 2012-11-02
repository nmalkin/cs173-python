#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt"
         "util.rkt"
         (typed-in racket/base (hash-copy : ((hashof 'a 'b) -> (hashof 'a 'b))))
         (typed-in racket/base (expt : (number number -> number)))
         (typed-in racket/base (quotient : (number number -> number)))
         (typed-in racket/base (string<? : (string string -> boolean)))
         (typed-in racket/base (string>? : (string string -> boolean)))
         (typed-in racket/base (string<=? : (string string -> boolean)))
         (typed-in racket/base (string>=? : (string string -> boolean)))
         (typed-in racket/base (for-each : (('a -> void) (listof number) -> 'b)))
         (typed-in racket/base (raise-user-error : ('symbol string -> 'a))))

;; interp-env : CExpr * Env * Store -> Result
(define (interp-env [expr : CExpr] [env : Env] [sto : Store]) : Result
  (type-case CExpr expr
    [CNum (n) (v*s*e (VNum n) sto env)]
    [CStr (s) (v*s*e (VStr s) sto env)]
    [CTrue () (v*s*e (VTrue) sto env)]
    [CFalse () (v*s*e (VFalse) sto env)]
    [CNone () (v*s*e (VNone) sto env)]

    [CClass (bases body)
               (type-case Result (interp-env body (cons (hash empty) env) sto)
                 [v*s*e (vbody sbody ebody)
                        (v*s*e (VClass bases (first ebody)) sbody ebody)]
                 [else (error 'interp "'return' outside of function")])]
    
    ;[CObject (
    
    [CSeq (e1 e2) (type-case Result (interp-env e1 env sto)
                    [v*s*e (v1 s1 new-env) (interp-env e2 new-env s1)]
                    [Return (v1 s1 new-env) (Return v1 s1 new-env)])]
    
    ;; note that for now we're assuming that dict keys and values aren't going
    ;; to mess with the environment and store, but this might be wrong
    [CDict (contents) (v*s*e
                        (VDict (lists->hash 
                               (map (lambda(k) (v*s*e-v (interp-env k env sto)))
                                    (hash-keys contents))
                               (map (lambda(k) (v*s*e-v (interp-env (some-v (hash-ref contents k))
                                                           env sto)))
                                    (hash-keys contents))))
                        sto
                        env)]

    ;; deal with pythonic scope here
    ;; only for ids!
    [CAssign (t v) 
             (type-case Result (interp-env v env sto)
               [v*s*e (vv sv venv) 
                  (type-case (optionof Address) (hash-ref (first env) (CId-x t)) 
                     [some (w) (begin 
                                 (v*s*e (VNone) 
                                        (hash-set sv w vv) 
                                        (cons 
                                          (hash-set (first venv)
                                                    (CId-x t) w)
                                          (rest venv))))] 
                     [none () (let ([w (new-loc)]) 
                                (begin 
                                  (v*s*e (VNone) 
                                         (hash-set sv w vv) 
                                         (cons 
                                           (hash-set 
                                             (immutable-hash-copy 
                                               (first env)) (CId-x t) w)
                                           (rest env)))))])]
               [else (error 'interp "'return' outside of function")])]
    
    [CError (e) (type-case Result (interp-env e env sto)
                  [v*s*e (ve se ee)
                         (raise-user-error 'interp (pretty ve))]
                  [else (error 'interp "'return' outside of function")])]

    [CIf (i t e) (type-case Result (interp-env i env sto)
                   [v*s*e (vi si envi) (type-case CVal (truthy? vi)
                                     [VTrue () (interp-env t envi si)]
                                     [else (interp-env e envi si)])]
                   [else (error 'interp "'return' outside of function")])]

    [CId (x) (fetch (lookup x env) sto env)]

    [CLet (x bind body)
          (letrec ([w (new-loc)])
            (type-case Result (interp-env bind env sto)
              [v*s*e (vb sb eb)
                     (interp-env body
                                 (cons (hash-set (first eb) x w) (rest eb))
                                 (hash-set sb w vb))]
              [else (error 'interp "'return' outside of function")]))]

    [CApp (fun arges)
     (type-case Result (interp-env fun env sto)
       [v*s*e 
        (vfun sfun efun) 
        (type-case CVal vfun
          [VClosure (cenv argxs body)
                    (let ([sa sfun])
                      (local [(define argvs 
                                (map (lambda (e) 
                                       (type-case Result (interp-env e efun sa)
                                         [v*s*e (varg sarg envarg) 
                                                (begin 
                                                  (set! sa sarg)
                                                  varg)]
                                         [else (error 'interp "'return' outside of function")])) arges))]
                        (local [(define-values (e s) (bind-args argxs argvs cenv sa))]
                          (type-case Result (interp-env body e s)
                            [v*s*e (vb sb eb) (v*s*e vb sb env)]
                            [Return (vb sb eb) (v*s*e vb sb env)]))))]
          [VClass (b d)
                  (let ([f (get-function '__init__ vfun efun sfun)]
                        [o (new-object vfun efun sfun)])
                    (type-case CVal f
                      [VClosure (cenv argxs body)
                         (let ([sa sfun])
                         (local [(define argvs 
                                   (map (lambda (e) 
                                          (type-case Result (interp-env e efun sa)
                                            [v*s*e (varg sarg envarg) 
                                                   (begin 
                                                     (set! sa sarg)
                                                     varg)]
                                            [else (error 'interp "'return' outside of function")])) arges))]
                           (local [(define-values (e s) 
                                     (bind-args argxs (cons o argvs) cenv sa))]
                             (type-case Result (interp-env body e s)
                               [v*s*e (vb sb eb) (v*s*e vb sb env)]
                               [Return (vb sb eb) (v*s*e vb sb env)]))))]
                      [else (error 'interp 
                                   "__init__ not found for the given class")]))]
          [else (error 'interp "Not a closure or constructor")])]
       [else (error 'interp "'return' outside of function")])]

    ;; lambdas for now, implement real functions later
    [CFunc (args body) (v*s*e (VClosure (cons (hash empty) env) args body) sto env)]
    
    [CReturn (value) (type-case Result (interp-env value env sto)
                       [v*s*e (vv sv ev) (Return vv sv ev)]
                       [else (error 'interp "'return' outside of function")])]

    [CPrim1 (prim arg)
            (type-case Result (interp-env arg env sto)
              [v*s*e (varg sarg envarg) 
                   (case prim
                     ['Invert (type-case CVal varg
                                [VNum (n) (v*s*e (VNum (- 0 (+ n 1))) sarg envarg)]
                                [else (error 'interp "Bad arguments to ~")])]
                     ['Not (type-case CVal (truthy? varg)
                             [VTrue () (v*s*e (VFalse) sarg envarg)]
                             [else (v*s*e (VTrue) sarg envarg)])]
                     ['USub (interp-env (CPrim2 'Sub (CNum 0) arg) env sarg)]
                     ['UAdd (interp-env (CPrim2 'Add (CNum 0) arg) env sarg)]
                     [else (v*s*e (python-prim1 prim varg) sarg envarg)])]
              [else (error 'interp "'return' outside of function")])]
    
    ;; implement this
    [CPrim2 (prim arg1 arg2) (interp-cprim2 prim arg1 arg2 sto env)]
    
    [else (error 'interp "haven't implemented a case yet")]))

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

;; handles lookup chain for function calls on objects
;; looks in object dict, then class dict, then base class dicts, then default class
;; order in which base class dicts are traversed depends on truth value of super
;; depth-first, left-to-right if super = #f
;; left-to-right, depth-second if super = #t
(define (get-function [n : symbol] [c : CVal] [e : Env] [s : Store]) : CVal
  (type-case CVal c
    [VClass (b d) 
	    (let ([w (hash-ref (VClass-dict c) n)])
              (type-case (optionof Address) (hash-ref (VClass-dict c) n)
                [some (w) (v*s*e-v (fetch w s e))]
                [none () (cond 
                        [(empty? b) (error 'interp 
                                           (string-append "Function not found: " 
                                                          (symbol->string n)))]
                        [else (get-function n 
                                            (v*s*e-v (fetch (lookup (first b) e) s e))
                                            e 
                                            s)])]))]
    [else (error 'interp "Not an object with functions.")]))

(define (new-object [c : CVal] [e : Env] [s : Store])
  (type-case CVal c
    [VClass (b d) (VObject c (hash empty))]
    [else (VObject (v*s*e-v (fetch (lookup 'object e) s e)) (hash empty))]))

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
    [VClosure (e a b) (VTrue)]
    [VDict (c) (if (empty? (hash-keys c))
                           (VTrue)
                           (VFalse))]
    [else (VFalse)]))

(define (interp expr)
  (type-case Result (interp-env expr (list (hash (list))) (hash (list)))
    [v*s*e (vexpr sexpr env) (if (not (VNone? vexpr)) 
                         (begin (display (pretty vexpr)) 
                                (display "\n"))
                         (display ""))]
    [else (error 'interp "'return' outside of function")]))

(define (interp-cprim2 [prim : symbol] 
                       [arg1 : CExpr]
                       [arg2 : CExpr]
                       [sto : Store]
                       [env : Env]) : Result

    (type-case Result (interp-env arg1 env sto)
      [v*s*e (varg1 sarg1 envarg1)
           (type-case Result (interp-env arg2 envarg1 sarg1)
             [v*s*e (varg2 sarg2 envarg2) 
                  (case prim
                    ['Add (cond 
                            [(and (VNum? varg1) (VNum? varg2))
                             (v*s*e (VNum (+ (VNum-n varg1) (VNum-n varg2))) 
                                    sarg2 envarg2)]
                            [(and (VStr? varg1) (VStr? varg2)) 
                             (v*s*e (VStr (string-append 
                                            (VStr-s varg1) 
                                            (VStr-s varg2)))
                                    sarg2 envarg2)]
                            [else (error 'interp "Bad arguments to +")])]

                    ['Sub (cond
                            [(and (VNum? varg1) (VNum? varg2)) 
                             (v*s*e (VNum (- (VNum-n varg1) (VNum-n varg2))) 
                                    sarg2 envarg2)]
                            [else (error 'interp "Bad arguments to -")])]

                    ['Mult (cond
                             [(and (VNum? varg1) (VNum? varg2)) 
                              (v*s*e (VNum (* (VNum-n varg1) (VNum-n varg2))) 
                                     sarg2 envarg2)]

                             [(and (VStr? varg1) (VNum? varg2))
                              (v*s*e (VStr (let ([count (VNum-n varg2)]
                                                 [s (VStr-s varg1)]
                                                 [ret ""])
                                             (begin
                                               (for-each 
                                                (lambda (i) (set! ret (string-append ret s)))
                                                (build-list count identity))
                                               ret))) sarg2 envarg2)]
                             [(and (VNum? varg1) (VStr? varg2))
                              (v*s*e (VStr (let ([count (VNum-n varg1)]
                                                 [s (VStr-s varg2)]
                                                 [ret ""])
                                             (begin
                                               (for-each 
                                                (lambda (i) (set! ret (string-append ret s)))
                                                (build-list count identity))
                                               ret))) sarg2 envarg2)]
                             [else (error 'interp "Bad arguments to *")])]

                    ;; need to make sure division is implemented correctly
                    ['Div (cond
                             [(and (VNum? varg1) (VNum? varg2) (< 0 (VNum-n varg2)))
                              (v*s*e (VNum (/ (VNum-n varg1) (VNum-n varg2)))
                                     sarg2 envarg2)]
                             [else (error 'interp "Bad arguments to /")])]

                    ['Mod (cond
                            [(and (VNum? varg1) (VNum? varg2) (< 0 (VNum-n varg2)))
                             (v*s*e (VNum (modulo (VNum-n varg1) (VNum-n varg2))) sarg2 envarg2)]

                            [else (error 'interp "Bad arguments to %")])]

                    ['Pow (cond
                            [(and (VNum? varg1) (VNum? varg2)) 
                             (v*s*e (VNum (expt (VNum-n varg1) (VNum-n varg2))) 
                                    sarg2 envarg2)]
                            [else (error 'interp "Bad arguments to **")])]

                    ['FloorDiv (cond
                                 [(and (VNum? varg1) (VNum? varg2)
                                       (< 0 (VNum-n varg2))) 
                                  (v*s*e (VNum (quotient (VNum-n varg1) (VNum-n varg2))) sarg2 envarg2)]
                                 [else (error 'interp "Bad arguments to //")])]

                    ['LShift (cond
                               [(and (VNum? varg1) (VNum? varg2)) 
                                (v*s*e (VNum (* (VNum-n varg1) (expt 2 (VNum-n varg2)))) sarg2 envarg2)]
                               [else (error 'interp "Bad arguments to <<")])]

                    ['RShift (cond
                               [(and (VNum? varg1) (VNum? varg2)) 
                                (v*s*e (VNum (quotient (VNum-n varg1) 
                                                       (expt 2 (VNum-n varg2)))) sarg2 envarg2)] 
                               [else (error 'interp "Bad arguments to >>")])]

                    ['Lt (cond
                          [(and (VNum? varg1) (VNum? varg2)) 
                           (v*s*e (if (< (VNum-n varg1) (VNum-n varg2))
                                    (VTrue)
                                    (VFalse)) sarg2 envarg2)]

                          [(and (VStr? varg1) (VStr? varg2))
                           (v*s*e (if (string<? (VStr-s varg1) (VStr-s varg2)) 
                                    (VTrue)
                                    (VFalse))
                                  sarg2 envarg2)]
                          [else (error 'interp "Bad arguments to <")])]
                
                    ['Gt (cond
                          [(and (VNum? varg1) (VNum? varg2)) 
                           (v*s*e (if (> (VNum-n varg1) (VNum-n varg2))
                                    (VTrue)
                                    (VFalse)) 
                                  sarg2 envarg2)]
                          [(and (VStr? varg1) (VStr? varg2))
                           (v*s*e (if (string>? (VStr-s varg1) (VStr-s varg2)) 
                                    (VTrue) 
                                    (VFalse)) 
                                  sarg2 envarg2)]
                          [else (error 'interp "Bad arguments to >")])]

                    ['LtE (cond
                          [(and (VNum? varg1) (VNum? varg2)) 
                           (v*s*e (if (<= (VNum-n varg1) (VNum-n varg2)) 
                                    (VTrue) 
                                    (VFalse)) 
                                  sarg2 envarg2)]
                          [(and (VStr? varg1) (VStr? varg2)) 
                           (v*s*e (if (string<=? (VStr-s varg1) (VStr-s varg2)) 
                                    (VTrue) 
                                    (VFalse))
                                  sarg2 envarg2)]
                          [else (error 'interp "Bad arguments to <=")])]

                    ['GtE (cond
                          [(and (VNum? varg1) (VNum? varg2)) 
                           (v*s*e (if (>= (VNum-n varg1) (VNum-n varg2)) 
                                    (VTrue) 
                                    (VFalse))
                                  sarg2 envarg2)]
                          [(and (VStr? varg1) (VStr? varg2)) (v*s*e (if (string>=? (VStr-s varg1) (VStr-s varg2))
                                                                      (VTrue)
                                                                      (VFalse)) sarg2 envarg2)]
                          [else (error 'interp "Bad arguments to >=")])]

                    ['Eq (cond
                          [(and (VNum? varg1) (VNum? varg2)) 
                           (v*s*e (if (= (VNum-n varg1) (VNum-n varg2)) 
                                    (VTrue) 
                                    (VFalse)) 
                                  sarg2 envarg2)]
                          [(and (VStr? varg1) (VStr? varg2)) 
                           (v*s*e (if (string=? (VStr-s varg1) (VStr-s varg2)) 
                                    (VTrue) 
                                    (VFalse)) 
                                  sarg2 envarg2)]
                          [else (error 'interp "Bad arguments to ==")])]
                    ['NotEq (cond
                          [(and (VNum? varg1) (VNum? varg2)) 
                           (v*s*e (if (not (= (VNum-n varg1) (VNum-n varg2))) 
                                    (VTrue) 
                                    (VFalse))
                                  sarg2 envarg2)]
                          [(and (VStr? varg1) (VStr? varg2))
                           (v*s*e (if (not (string=? (VStr-s varg1) (VStr-s varg2))) 
                                    (VTrue) 
                                    (VFalse)) 
                                  sarg2 envarg2)]
                          [else (error 'interp "Bad arguments to !=")])]

                    ;; Handle Is, IsNot, In, NotIn
                    [else (error 'interp (string-append "Haven't implemented a
                                                        case yet: "
                                                        (symbol->string
                                                          prim)))])]
             [else (error 'interp "'return' outside of function")])]
      [else (error 'interp "'return' outside of function")]))
