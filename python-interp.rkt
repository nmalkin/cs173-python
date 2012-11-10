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

;; interp-cascade, interprets a list of expressions with an initial store,
;; environment and produces the list of values and the final environment and
;; store using the values/define-values 
(define (interp-cascade [exprs : (listof CExpr)] 
                        [init-s : Store]
                        [init-e : Env])
  (local [(define (rec-cascade exprs e s)
            (cond [(empty? exprs) empty]
                  [(cons? exprs) (let ([first-r (interp-env (first exprs)
                                                           e s)])
                                   (cons first-r
                                         (rec-cascade (rest exprs)
                                                      (v*s*e-e first-r)
                                                      (v*s*e-s first-r))))]))
         (define result-list (rec-cascade exprs init-e init-s))]

         (values (map v*s*e-v result-list) 
                 (v*s*e-s (first (reverse result-list)))
                 (v*s*e-e (first (reverse result-list))))))


;; interp-env : CExpr * Env * Store -> Result
(define (interp-env [expr : CExpr] [env : Env] [sto : Store]) : Result
    (type-case CExpr expr
    [CStr (s) (v*s*e (VStr s) sto env)]
    [CTrue () (v*s*e (VTrue) sto env)]
    [CFalse () (v*s*e (VFalse) sto env)]
    [CNone () (v*s*e (VNone) sto env)]

    [CClass (name base body)
               (type-case Result (interp-env body (cons (hash empty) env) sto)
                 [v*s*e (vbody sbody ebody)
                        (v*s*e (VObject base 
                                        (some (MetaClass name)) 
                                        (first ebody)) 
                               sbody ebody)]
                 [else (error 'interp "'return' outside of function")])]
    
    [CGetField (value attr)
	       (type-case Result (interp-env value env sto)
		 [v*s*e (vval sval eval)
			(v*s*e (get-field attr vval eval sval) sval eval)]
		 [else (error 'interp "'return' outside function")])]
			
    
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
		      (type-case CExpr t
			[CId (x) (assign-to-id t vv venv sv)]
			[CGetField (o a) (assign-to-field o a vv venv sv)]
			[else (error 'interp "Can only assign to ids or objects")])]
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

    [CObject (c mval) (v*s*e (VObject c mval (make-hash empty))
                             sto
                             env)]

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
                            [v*s*e (vb sb eb) (v*s*e (VNone) sb env)]
                            [Return (vb sb eb) (v*s*e vb sb env)]))))]
          [VObject (b mval d)
                   (if (and (some? mval) (MetaClass? (some-v mval)))
                      (let ([f (get-field '__init__ vfun efun sfun)]
                            [o (new-object (MetaClass-c (some-v mval)) efun sfun)])
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
                                   [v*s*e (vb sb eb) (v*s*e 
                               (let ([obj (v*s*e-v 
                                        (fetch (lookup (first argxs)
                                               eb) sb eb))])
                                        obj)
                               sb env)]
                                   [Return (vb sb eb) (v*s*e vb sb env)]))))]
                          [else (error 'interp 
                                       "__init__ not found for the given
                                       class")]))
                                         
                          (error 'interp "I dont know how to call objects yet, bro"))]
          [else (error 'interp "Not a closure or constructor")])]
       [else (error 'interp "'return' outside of function")])]

    [CFunc (args body) (v*s*e (VClosure (cons (hash empty) env) args body) sto env)]
    
    [CReturn (value) (type-case Result (interp-env value env sto)
                       [v*s*e (vv sv ev) (Return vv sv ev)]
                       [else (error 'interp "'return' outside of function")])]

    [CPrim1 (prim arg)
            (type-case Result (interp-env arg env sto)
              [v*s*e (varg sarg envarg) 
                   (case prim
                     ['Not (type-case CVal (truthy? varg)
                             [VTrue () (v*s*e (VFalse) sarg envarg)]
                             [else (v*s*e (VTrue) sarg envarg)])]
                     ;['USub (interp-env (CPrim2 'Sub (CNum 0) arg) env sarg)]
                     ;['UAdd (interp-env (CPrim2 'Add (CNum 0) arg) env sarg)]
                     [else (v*s*e (python-prim1 prim varg) sarg envarg)])]
              [else (error 'interp "'return' outside of function")])]
    
    ;; implement this
    [CPrim2 (prim arg1 arg2) (interp-cprim2 prim arg1 arg2 sto env)]
    
    [CBuiltinPrim (op args) (local [(define-values (val-list new-s new-e)
                                      (interp-cascade args sto env))
                                    (define mayb-val (builtin-prim op val-list))] 

                                   (if (some? mayb-val)
                                       (v*s*e (some-v mayb-val)
                                              new-s
                                              new-e)
                                       ;; todo: real exceptions
                                       (error 'interp (string-append "Builtin error for "
                                                                     (symbol->string
                                                                       op)))))]))

    ;[else (error 'interp "haven't implemented a case yet")]))

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

(define (assign-to-id id v e s)
  (type-case (optionof Address) (hash-ref (first e) (CId-x id)) 
    [some (w) (begin 
               (v*s*e (VNone) 
                      (hash-set s w v) 
                      (cons 
                        (hash-set (first e)
                                  (CId-x id) w)
                        (rest e))))] 
    [none () (let ([w (new-loc)]) 
               (begin 
                (v*s*e (VNone) 
                       (hash-set s w v) 
                       (cons 
                         (hash-set 
                           (immutable-hash-copy 
                             (first e)) (CId-x id) w)
                         (rest e)))))]))

;; handles lookup chain for function calls on objects
;; looks in object dict, then class dict, then base class dicts, then default class
;; order in which base class dicts are traversed depends on truth value of super
;; depth-first, left-to-right if super = #f
;; left-to-right, depth-second if super = #t
(define (get-field [n : symbol] [c : CVal] [e : Env] [s : Store]) : CVal
  (type-case CVal c
    [VObject (antecedent mval d) 
             (let ([w (hash-ref (VObject-dict c) n)])
              (type-case (optionof Address) (hash-ref (VObject-dict c) n)
                [some (w) (v*s*e-v (fetch w s e))]
                [none () (let ([base (v*s*e-v (fetch (lookup antecedent e) s e))])
                           (cond 
                             [(VNone? base) (error 'interp 
                                                   (string-append "Function not found: " 
                                                          (symbol->string n)))]
                             [else (get-field n base e s)]))]))]
    [else (error 'interp "Not an object with functions.")]))

(define (assign-to-field o f v e s)
  (type-case Result (interp-env o e s)
    [v*s*e (vo so eo) (type-case CVal vo
	[VObject (ante-name mval d)
	  (let ([w (hash-ref (VObject-dict vo) f)])
	    (type-case (optionof Address) (hash-ref (VObject-dict vo) f)
	      [some (w) (v*s*e (VNone)
			       (hash-set so w v)
			       eo)]
	      [none () (let ([w (new-loc)])
			   (let ([nw (hash-ref (first eo) (CId-x o))])
			     (let ([snew (hash-set so (some-v nw) 
			                 (VObject ante-name
                                                  mval
						  (hash-set (VObject-dict vo) f w)))])
      		           	(v*s*e (VNone)
			          (hash-set snew w v)
			          eo))))]))]
    	[else (error 'interp "Can't assign to nonobject.")])]
    [else (error 'interp "'return' outside function")]))

(define (new-object [c-name : symbol] [e : Env] [s : Store])
  (VObject c-name (none) (hash empty)))
 

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
                    ;; Handle Is, IsNot, In, NotIn
                    [else (error 'interp (string-append "Haven't implemented a
                                                        case yet: "
                                                        (symbol->string
                                                          prim)))])]
             [else (error 'interp "'return' outside of function")])]
      [else (error 'interp "'return' outside of function")]))
