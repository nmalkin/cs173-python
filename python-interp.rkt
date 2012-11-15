#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt"
         "builtins/object.rkt"
         "builtins/bool.rkt"
         "util.rkt"
         (typed-in racket/base (hash-copy : ((hashof 'a 'b) -> (hashof 'a 'b))))
         (typed-in racket/base (expt : (number number -> number)))
         (typed-in racket/base (quotient : (number number -> number)))
         (typed-in racket/string (string-join : ((listof string) string -> string)))
         (typed-in racket/base (string<? : (string string -> boolean)))
         (typed-in racket/base (string>? : (string string -> boolean)))
         (typed-in racket/base (string<=? : (string string -> boolean)))
         (typed-in racket/base (string>=? : (string string -> boolean)))
         (typed-in racket/base (for-each : (('a -> void) (listof number) -> 'b)))
         (typed-in racket/base (raise-user-error : (string -> 'a))))


;; interp-cascade, interprets a list of expressions with an initial store,
;; environment and produces the list of values and the final environment and
;; store using the values/define-values 
(define (interp-cascade [exprs : (listof CExpr)] 
                        [init-s : Store]
                        [init-e : Env]) : ((listof CVal) * Store * Env)
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
                 (if (cons? result-list)
                     (v*s*e-s (first (reverse result-list)))
                     init-s)
                 (if (cons? result-list)
                     (v*s*e-e (first (reverse result-list)))
                     init-e))))


;; interp-env : CExpr * Env * Store -> Result
(define (interp-env [expr : CExpr] [env : Env] [sto : Store]) : Result
    (type-case CExpr expr
    [CStr (s) (v*s*e (VObject 'str (some (MetaStr s)) (hash empty)) sto env)]
    [CTrue () (v*s*e true-val sto env)]
    [CFalse () (v*s*e false-val sto env)]
    [CNone () (v*s*e (VNone) sto env)]

    [CClass (name base body)
               (type-case Result (interp-env body (cons (hash empty) env) sto)
                 [v*s*e (vbody sbody ebody)
                        (v*s*e (VObject base 
                                        (some (MetaClass name)) 
                                        (first ebody)) 
                               sbody env)]
                 [Return (vval sval eval) (return-exception eval sval)]
                 [Exception (vval sval eval) (Exception vval sval eval)])]
    
    [CGetField (value attr)
	       (type-case Result (interp-env value env sto)
                    [v*s*e (vval sval eval)
                           (v*s*e (get-field attr vval eval sval) sval eval)]
                    [Return (vval sval eval) (return-exception eval sval)]
                    [Exception (vval sval eval) (Exception vval sval eval)])]
			
    [CSeq (e1 e2) (type-case Result (interp-env e1 env sto)
                    [v*s*e (v1 s1 new-env) (interp-env e2 new-env s1)]
                    [Return (v1 s1 new-env) (Return v1 s1 new-env)]
                    [Exception (v1 s1 new-env) (Exception v1 s1 new-env)])]
    
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

    [CList (values)
           (local [(define-values (val-list new-s new-e)
                                      (interp-cascade values sto env))]
               (v*s*e (VObject 'list
                                (some (MetaList val-list))
                                (make-hash empty))
                      new-s
                      new-e))]

    [CTuple (values)
           (local [(define-values (val-list new-s new-e)
                                      (interp-cascade values sto env))]
               (v*s*e (VObject 'tuple
                                (some (MetaTuple val-list))
                                (make-hash empty))
                      new-s
                      new-e))]

    ;; deal with pythonic scope here
    ;; only for ids!
    [CAssign (t v) 
             (type-case Result (interp-env v env sto)
               [v*s*e (vv sv venv) 
                     (type-case CExpr t
                                [CId (x) (assign-to-id t vv venv sv)]
                                [CGetField (o a) (assign-to-field o a vv venv sv)]
                                [else (mk-exception 'SyntaxError
                                                 "can't assign to literals"
                                                 venv
                                                 sv)])]
               [Return (vv sv ev) (return-exception ev sv)]
               [Exception (vv sv ev) (Exception vv sv ev)])]
    
    [CError (e) (type-case Result (interp-env e env sto)
                  [v*s*e (ve se ee)
                         (raise-user-error (string-append "CERROR: " (pretty ve)))]
                  [Return (ve se ee) (return-exception ee se)]
                  [Exception (ve se ee) (Exception ve se ee)])]

    [CIf (i t e) (type-case Result (interp-env i env sto)
                       [v*s*e (vi si envi) (if (truthy? vi)
                                             (interp-env t envi si)
                                             (interp-env e envi si))]
                       [Return (vi si envi) (return-exception envi si)]
                       [Exception (vi si envi) (Exception vi si envi)])]

    [CId (x) (let ([w (lookup x env)])
               (if (none? w)
                 (mk-exception 'NameError 
                                (string-append "name '" 
                                               (string-append (symbol->string x)
                                                              "' is not defined"))
                                env sto)
                 (v*s*e (fetch (some-v w) sto)
                        sto
                        env)))]

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
              [Return (vb sb eb) (return-exception eb sb)]
              [Exception (vb sb eb) (Exception vb sb eb)]))]

    [CApp (fun arges)
     (type-case Result (interp-env fun env sto)
       [v*s*e 
        (vfun sfun efun) 
        (type-case CVal vfun
          [VClosure (cenv argxs body)
                      (local [(define-values (argvs sc ec) (interp-cascade arges sfun efun))
                              (define-values (e s) (bind-args argxs argvs arges efun cenv sc))]
                          (type-case Result (interp-env body e s)
                            [v*s*e (vb sb eb) (v*s*e (VNone) sb env)]
                            [Return (vb sb eb) (v*s*e vb sb env)]
                            [Exception (vb sb eb) (Exception vb sb env)]))]
          [VObject (b mval d)
                   (if (and (some? mval) (MetaClass? (some-v mval)))
                      ; We're calling a class.
                            ; Get its constructor
                      (let ([f (get-field '__init__ vfun efun sfun)]
                            ; Create an empty object. This will be the instance of that class.
                            [o (new-object (MetaClass-c (some-v mval)) efun sfun)])
                        (type-case CVal f
                          [VClosure (cenv argxs body)
                                    ; interpret the arguments to the constructor
                             (local [(define-values (argvs sc ec)
                                       (interp-cascade arges sfun efun))
                                     ; bind the (interpreted) arguments to the constructor
                                     (define-values (e s) 
                                         (bind-args argxs (cons o argvs) 
                                                    (cons (CId 'init) arges) efun cenv sc))]
                                        ; interpret the constructor body
                                        (type-case Result (interp-env body e s)
                                          [v*s*e (vb sb eb) (v*s*e 
                                             (let ([obj (fetch (some-v 
                                                                 (lookup (first argxs)
                                                                         eb)) sb)])
                                               obj)
                                             sb env)]
                                          [Return (vb sb eb) (v*s*e vb sb env)]
                                          [Exception (vb sb eb) (Exception vb sb env)]))]
                          [else (error 'interp 
                                       "__init__ not found. THIS SHOULDN'T HAPPEN.")]))
                                         
                      (error 'interp "I dont know how to call objects yet, bro"))]
          [else (error 'interp "Not a closure or constructor")])]
       [Return (vfun sfun efun) (return-exception efun sfun)]
       [Exception (vfun sfun efun) (Exception vfun sfun efun)])]

    [CFunc (args body) (v*s*e (VClosure (cons (hash empty) env) args body) sto env)]
    
    [CReturn (value) (type-case Result (interp-env value env sto)
                       [v*s*e (vv sv ev) (Return vv sv ev)]
                       [Return (vv sv ev) (return-exception ev sv)]
                       [Exception (vv sv ev) (Exception vv sv ev)])]

    [CPrim1 (prim arg)
            (type-case Result (interp-env arg env sto)
              [v*s*e (varg sarg envarg) 
                   (case prim
                     ['Not (if (truthy? varg)
                             (v*s*e false-val sarg envarg)
                             (v*s*e true-val sarg envarg))]
                     [else (v*s*e (python-prim1 prim varg) sarg envarg)])]
              [Return (varg sarg earg) (return-exception earg sarg)]
              [Exception (varg sarg earg) (Exception varg sarg earg)])]
    
    ;; implement this
    [CPrim2 (prim arg1 arg2) (interp-cprim2 prim arg1 arg2 sto env)]
    
    [CBuiltinPrim (op args) (local [(define-values (val-list new-s new-e)
                                      (interp-cascade args sto env))
                                    (define mayb-val (builtin-prim op val-list
                                                                   env sto))] 

                                   (if (some? mayb-val)
                                       (v*s*e (some-v mayb-val)
                                              new-s
                                              new-e)
                                       ;; todo: real exceptions
                                       (error 'interp (string-append "Builtin error for "
                                                                     (symbol->string
                                                                       op)))))]
    [CRaise (expr) (type-case Result (interp-env expr env sto)
                     [v*s*e (vexpr sexpr eexpr) 
                            (mk-exception 'TypeError
                                       "exceptions must derive from BaseException"
                                       eexpr
                                       sexpr)]
                     [Return (vexpr sexpr eexpr) (return-exception eexpr sexpr)]
                     [Exception (vexpr sexpr eexpr) (Exception vexpr sexpr eexpr)])]
    
    [CTryExceptElseFinally (try excepts orelse finally)
         (type-case Result (interp-env try env sto)
            [v*s*e (vtry stry etry)
                   (type-case Result (interp-env orelse env sto)
                      [v*s*e (velse selse eelse)
                             (type-case Result (interp-env finally env sto)
                                [v*s*e (vfin sfin efin)
                                       (v*s*e (VNone) sfin efin)]
                                [Return (vfin sfin efin) (return-exception efin sfin)]
                                [Exception (vfin sfin efin)
                                           (Exception vfin sfin efin)])]
                      [Return (velse selse eelse) (return-exception eelse selse)]
                      [Exception (velse selse eelse)
                                 (Exception velse selse eelse)])]
            [Return (vtry stry etry) (return-exception etry stry)]
            [Exception (vtry stry etry) (Exception vtry stry etry)])]
                       ;; handle excepts here
    
    [CExcept (types body) (error 'interp "WTF is an except?")]))

    ;[else (error 'interp "haven't implemented a case yet")]))

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
              (type-case (optionof Address) w
                [some (w) (fetch w s)]
                [none () (let ([base (fetch (some-v (lookup antecedent e)) s)])
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
	      [some (w) (v*s*e (VNone) (hash-set so w v) eo)]
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
    [Return (vo so eo) (return-exception eo so)]
    [Exception (vo so eo) (Exception vo so eo)]))

(define (new-object [c-name : symbol] [e : Env] [s : Store])
  (VObject c-name (none) (hash empty)))

(define (bind-args [args : (listof symbol)] [vals : (listof CVal)] 
                   [arges : (listof CExpr)] [env : Env] [ext : Env]
                   [sto : Store]) : (Env * Store)
  (cond [(and (empty? args) (empty? vals)) (values ext sto)]
        [(or (empty? args) (empty? vals))
         (error 'interp "Arity mismatch")]
        [(and (cons? args) (cons? vals))
         (let ([val (first vals)]
               [where -1]
               [mutability-check (lambda ()
                    (type-case CExpr (first arges)
                         [CId (x)
                              (if (symbol=? x 'init)
                                  (new-loc)
                                  (some-v (lookup x env)))]
                         [else (new-loc)]))])
            (begin
              (type-case CVal val
              [VObject (ante-name mayb-mval dict)
                       ;; these should not get new store locations if they already exist
                       (if (some? mayb-mval)
                         (let ([mval (some-v mayb-mval)])
                           (type-case MetaVal mval
                             [MetaClass (c) (set! where (mutability-check))]
                             [MetaList (l) (set! where (mutability-check))]
                             ;;[MetaDict (d) (;; get loc of val in store)]
                             ;; immutable types should get a new store location
                             [else (set! where (new-loc))]))
                         (set! where (mutability-check)))]
              [else (set! where (new-loc))])
            (let ([e (cons (hash-set (first ext) (first args) where) (rest ext))]
                  [s (hash-set sto where (first vals))])
                 (bind-args (rest args) (rest vals) (rest arges) env e s))))]))

(define (make-exception [name : symbol] [error : string]) : CExpr
  (CApp
    (CId name)
    (list (CStr error))))

(define (mk-exception [type : symbol] [arg : string]
                      [env : Env] [sto : Store]) : Result
  (let ([exception 
          (interp-env (make-exception
                        type 
                        arg)
                      env sto)])
    (Exception (v*s*e-v exception)
               (v*s*e-s exception)
               (v*s*e-e exception))))

(define (return-exception [env : Env] [sto : Store]) : Result
  (mk-exception 'SyntaxError "'return' outside function" env sto))

(define (truthy? [val : CVal]) : boolean
  (type-case CVal val
    [VStr (s) (if (string=? "" s)
              false 
              true)]
    [VNone () false]
    [VClosure (e a b) true]
    [VObject (a mval d) (truthy-object? (VObject a mval d))]
    [VDict (c) (if (empty? (hash-keys c))
                           false
                           true)]))

(define (interp expr)
  (type-case Result (interp-env expr (list (hash (list))) (hash (list)))
    [v*s*e (vexpr sexpr env) (if (not (VNone? vexpr)) 
                         (begin (display (pretty vexpr)) 
                                (display "\n"))
                         (display ""))]
    [Return (vexpr sexpr env) (raise-user-error "SyntaxError: 'return' outside function")]
    [Exception (vexpr sexpr env) (raise-user-error
                                   (string-join
                                     (list
                                       (symbol->string (VObject-antecedent vexpr))
                                       (MetaStr-s
                                         (some-v 
                                           (VObject-mval
                                             (fetch (some-v
                                                      (hash-ref 
                                                        (VObject-dict vexpr) 'args))
                                                    sexpr)))))
                                     ": "))]))

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
                    ['Is (if (is? varg1 varg2)
                           (v*s*e true-val sarg2 envarg2)
                           (v*s*e false-val sarg2 envarg2))]
                    ['IsNot (if (not (is? varg1 varg2))
                           (v*s*e true-val sarg2 envarg2)
                           (v*s*e false-val sarg2 envarg2))]
                    [else (error 'interp (string-append "Haven't implemented a
                                                        case yet: "
                                                        (symbol->string
                                                          prim)))])]
             [Return (varg2 sarg2 envarg2) (return-exception envarg2 sarg2)]
             [Exception (varg2 sarg2 envarg2) (Exception varg2 sarg2 envarg2)])]
      [Return (varg1 sarg1 envarg1) (return-exception envarg1 sarg1)]
      [Exception (varg1 sarg1 envarg1) (Exception varg1 sarg1 envarg1)]))
