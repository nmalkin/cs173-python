#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt"
         "util.rkt"
         "builtins/num.rkt" 
         "builtins/str.rkt")
(require (typed-in racket/base (number->string : (number -> string)))
         (typed-in racket/base (append : ((listof 'a) (listof 'a) -> (listof 'a)))))



(define (desugar-boolop [op : symbol] [values : (listof PyExpr)]
                        [global? : boolean]
                        [env : IdEnv]) : DesugarResult
  (begin ;(display "binop: ") (display values) (display "\n\n")
  (local [(define first-val (rec-desugar (first values) global? env))]
    (if (> (length values) 1)
      (local [(define rest-val
                (desugar-boolop op (rest values) global? (DResult-env first-val)))]
        (case op
          ['And (DResult
                  (CIf (DResult-expr first-val)
                       (DResult-expr rest-val) 
                       (DResult-expr first-val))
                  (DResult-env rest-val))]
          ['Or (DResult
                 (CIf (DResult-expr first-val)
                      (DResult-expr first-val)
                      (DResult-expr rest-val))
                 (DResult-env rest-val))]))
    first-val))))

(define (desugar-compop [l : PyExpr] 
                        [ops : (listof symbol)] 
                        [comparators : (listof PyExpr)]
                        [global? : boolean]
                        [env : IdEnv]) : DesugarResult
  (begin ;(display "compop: ") (display comparators) (display "\n")
         ;(display ops) (display "\n")
         ;(display l) (display "\n")
         (local [;(define first-right (rec-desugar (first comparators) global? env))
                 ;(define l-expr (rec-desugar l global? (DResult-env first-right)))
                 (define first-comp (rec-desugar (PyBinOp l (first ops) (first
                                                                          comparators))
                                                 global? env))]
                (if (> (length comparators) 1) 
                  (local [(define rest-comp (desugar-compop (first comparators)
                                                            (rest ops)
                                                            (rest comparators)
                                                            global?
                                                            (DResult-env first-comp)))]
                         (DResult
                           (CIf (DResult-expr first-comp)
                                (DResult-expr rest-comp)
                                (DResult-expr first-comp))
                           (DResult-env rest-comp)))
                  first-comp))))

;; look through a  and find a list of all names from assignments and definition
;; if global scope, it only gets definitions, for local scope it gets
;; definitions and assignments
(define (get-names [expr : PyExpr] [global? : boolean]) : (listof symbol)
  (type-case PyExpr expr
   [PyIf (t b e) (get-names e global?)]
   [PySeq (es) (foldl (lambda(e so-far) (append (get-names e global?) so-far))
                      empty
                      es)]
   [PyId (id ctx) (list id)]
   [PyAssign (targets v) (if (not global?)
                           (foldl (lambda(t so-far) (append (get-names t global?)
                                                          so-far))
                                empty
                                targets)
                           empty)]
   [PyExcept (t body) (get-names body global?)]
   [PyTryExceptElseFinally (t e o f)
                           (append (get-names t global?)
                              (append (foldl (lambda(e so-far) (append
                                                                 (get-names e
                                                                            global?)
                                                                 so-far))
                                             empty 
                                             e)
                                (append (get-names o global?)
                                  (get-names f global?))))]
   [PyClass (name bases body) (list name)]
   [PyBinOp (l o r) (append (get-names l global?)
                      (get-names r global?))]
   [PyUnaryOp (o operand) (get-names operand global?)]
   [PyFunc (name args body) (list name)]
   [PyFuncVarArg (name args sarg body) (list name)]
   [else empty]))

(define (desugar-pymodule [es : (listof PyExpr)] 
                          [global? : boolean]
                          [env : IdEnv]) : DesugarResult
  (local [(define names  (get-names (PySeq es) global?))
          (define prelude 
            (if (not (empty? names))
              (rec-desugar (PySeq (map (lambda (n) (PyAssign (list (PyId n 'Load))
                                                             (PyUndefined)))
                                       names))
                           global? env)
              (rec-desugar (PyPass) global? env)))
          (define body (rec-desugar (PySeq es) global? (DResult-env prelude)))]
    (DResult
      (CModule
        (DResult-expr prelude)
        (DResult-expr body))
      (DResult-env body))))

(define (map-desugar [exprs : (listof PyExpr)]
                     [global? : boolean]
                     [env : IdEnv]): ((listof CExpr) * IdEnv)
  (local [(define (rec-map-desugar exps g e)
            (begin ;(display exps) (display "\n\n\n")
            (cond
              [(empty? exps) (values empty e)]
              [(cons? exps)
               (local [(define first-r (rec-desugar (first exps) g e))
                       (define-values (results last-env)
                         (rec-map-desugar (rest exps) g (DResult-env first-r)))]
                 (values
                   (cons (DResult-expr first-r)
                         results)
                   last-env))])))]
    (rec-map-desugar exprs global? env)))


;; for the body of some local scope level like a class or function, hoist
;; all the assignments and defs to the top as undefineds
(define (desugar-local-body [expr : PyExpr] [args : (listof symbol)] [env : IdEnv]) : DesugarResult
  (local [(define names (get-names expr false))]
    (rec-desugar
      (PySeq (append 
               (if (not (empty? names))
                 (map (lambda(n) (PyAssign (list (PyId n 'Load))
                                               (PyUndefined)))
                      (filter (lambda(n) (not (member n args)))
                          names)) 
                 (list (PyPass))) 
               (list expr)))
      false
      (extract-globals env))))

(define (rec-desugar [expr : PyExpr] [global? : boolean] [env : IdEnv]) : DesugarResult 
  (begin ;(display expr) (display "\n\n")
    (type-case PyExpr expr
    [PySeq (es) (local [(define-values (exprs-r last-env)
                          (map-desugar es global? env))]
                  (DResult
                    (foldl (lambda (e1 so-far) (CSeq so-far e1))
                           (first exprs-r)
                           (rest exprs-r))
                    last-env))]
    [PyModule (es) (desugar-pymodule es global? env)]
    [PyAssign (targets value) 
              (type-case PyExpr (first targets) ; TODO: deal with multiple targets (ignoring all but the first one for now)
                         ; We handle two kinds of assignments.
                         ; An assignment to a subscript is desugared as a __setattr__ call.
                         [PySubscript (left ctx slice)
                                      (letrec ([desugared-target (rec-desugar left global? env)]
                                               [desugared-slice (rec-desugar slice global? (DResult-env desugared-target))]
                                               [desugared-value (rec-desugar value global? (DResult-env desugared-slice))]
                                               [target-id (new-id)])
                                        (DResult
                                            (CLet target-id (DResult-expr desugared-target)
                                              (CApp (CGetField (CId target-id (LocalId)) '__setattr__)
                                                    (list (CId target-id (LocalId))
                                                          (DResult-expr desugared-slice)
                                                          (DResult-expr desugared-value))
                                                    (none)))
                                            (DResult-env desugared-value)))]
                         ; The others become a CAssign.
                         [else
                              (local [(define-values (targets-r mid-env)
                                        (map-desugar targets global? env))
                                     (define value-r (rec-desugar value global? mid-env))]
                                (DResult
                                  (foldl (lambda (t so-far) (CSeq so-far (CAssign t (DResult-expr value-r))))
                                         (CAssign (first targets-r) (DResult-expr value-r))
                                         (rest targets-r))
                                  (DResult-env value-r)))
                           ])]

    [PyNum (n) (DResult (make-builtin-num n) env)]
    [PySlice (lower upper step) (error 'desugar "Shouldn't desugar slice directly")]
    [PyBool (b) (DResult (if b (CTrue) (CFalse)) env)]
    [PyNone () (DResult (CNone) env)]
    [PyStr (s) (DResult (make-builtin-str s) env)]
    [PyId (x ctx) (DResult (CId x (LocalId)) env)]
    [PyGlobal (ids) (DResult (CNone) (add-ids ids (GlobalId) env))]
    [PyNonlocal (ids) (DResult (CNone) (add-ids ids (NonlocalId) env))]
    [PyUndefined () (DResult (CUndefined) env)]  

    ; for now just desugar raise as error
    ; TODO: implement real exceptions
    [PyRaise (expr) (local [(define expr-r (rec-desugar expr global? env))]
                            (DResult
                              (CRaise 
                                (if (PyPass? expr)
                                    (none)
                                    (some (DResult-expr expr-r))))
                              (DResult-env expr-r)))]

    ; PyPass is an empty lambda
    [PyPass () (DResult (CApp (CFunc empty (none) (CNone)) empty (none)) env)] 

    [PyIf (test body orelse)
          (local [(define test-r (rec-desugar test global? env))
                  (define body-r (rec-desugar body global? (DResult-env test-r)))
                  (define orelse-r (rec-desugar orelse global? (DResult-env body-r)))]
          (DResult 
            (CIf (DResult-expr test-r)
                 (DResult-expr body-r)
                 (DResult-expr orelse-r))
            (DResult-env orelse-r)))]

    [PyBinOp (left op right)
             (local [(define left-r (rec-desugar left global? env))
                     (define left-c (DResult-expr left-r))
                     (define right-r (rec-desugar right global? (DResult-env left-r)))
                     (define right-c (DResult-expr right-r))] 
               (case op 
                 ['Add (DResult (CApp (CGetField left-c '__add__) 
                                      (list left-c right-c)
                                      (none))
                                (DResult-env right-r))]
                 ['Sub (DResult (CApp (CGetField left-c '__sub__) 
                                      (list left-c right-c)
                                      (none))
                                (DResult-env right-r))]
                 ['Mult (DResult (CApp (CGetField left-c '__mult__)
                                       (list left-c right-c)
                                       (none))
                                 (DResult-env right-r))]
                 ['Div (DResult (CApp (CGetField left-c '__div__)
                                      (list left-c right-c)
                                      (none))
                                (DResult-env right-r))]
                 ['FloorDiv (DResult (CApp (CGetField left-c '__floordiv__)
                                           (list left-c right-c)
                                           (none))
                                     (DResult-env right-r))]
                 ['Mod (DResult (CApp (CGetField left-c '__mod__)
                                      (list left-c right-c)
                                      (none))
                                (DResult-env right-r))]
                 ['BitAnd (DResult (CApp (CGetField left-c '__and__)
                                (list left-c right-c)
                                (none))
                                (DResult-env right-r))]
                 ['BitOr (DResult (CApp (CGetField left-c '__or__)
                               (list left-c right-c)
                               (none))
                               (DResult-env right-r))]
                 ['BitXor (DResult (CApp (CGetField left-c '__xor__)
                                (list left-c right-c)
                                (none))
                                (DResult-env right-r))]
                 ['Eq (DResult (CApp (CGetField left-c '__eq__)
                                     (list left-c right-c)
                                     (none))
                               (DResult-env right-r))]
                 ['Gt (DResult (CApp (CGetField left-c '__gt__)
                                     (list left-c right-c)
                                     (none))
                               (DResult-env right-r))]
                 ['Lt (DResult (CApp (CGetField left-c '__lt__)
                                     (list left-c right-c)
                                     (none))
                               (DResult-env right-r))]
                 ['LtE (DResult (CApp (CGetField left-c '__lte__)
                                      (list left-c right-c)
                                      (none))
                                (DResult-env right-r))]
                 ['GtE (DResult (CApp (CGetField left-c '__gte__)
                                      (list left-c right-c)
                                      (none))
                                (DResult-env right-r))]
                 ['NotEq (rec-desugar (PyUnaryOp 'Not (PyBinOp left 'Eq right)) 
                                      global? env)]
                 ['In (DResult 
                        (CApp (CFunc (list 'self 'test) (none)
                                     (CSeq
                                       (CAssign (CId '__infunc__ (LocalId))
                                                (CGetField (CId 'self (LocalId))
                                                           '__in__))
                                       (CIf (CId '__infunc__ (LocalId))
                                            (CReturn
                                              (CApp
                                                (CId '__infunc__ (LocalId))
                                                (list (CId 'self (LocalId))
                                                      (CId 'test (LocalId)))
                                                (none)))
                                            (CApp (CId 'TypeError (LocalId))
                                                  (list (CObject
                                                          'str
                                                          (some (MetaStr 
                                                                  (string-append
                                                                    "argument of type '___'" 
                                                                    "is not iterable")))))
                                                  (none)))))
                              (list right-c left-c)
                              (none))
                        (DResult-env right-r))]
                 ['NotIn (rec-desugar (PyUnaryOp 'Not (PyBinOp left 'In right))
                                      global? env)]
                 [else (DResult
                         (CPrim2 op left-c right-c)
                         (DResult-env right-r))]))]

    [PyUnaryOp (op operand)
               (case op
                 ['USub (rec-desugar (PyBinOp (PyNum 0) 'Sub operand) global? env)]
                 ['UAdd (rec-desugar (PyBinOp (PyNum 0) 'Add operand) global? env)]
                 ['Invert (local [(define roperand (rec-desugar operand global? env))]
                            (DResult 
                              (CApp (CGetField (DResult-expr roperand) '__invrt__)
                                (list (DResult-expr roperand))
                                (none))
                              (DResult-env roperand)))]
                 [else (local [(define roperand (rec-desugar operand global? env))]
                              (DResult (CPrim1 op 
                                               (DResult-expr roperand)) 
                                       (DResult-env roperand)))])]

    [PyBoolOp (op values) (desugar-boolop op values global? env)]
              
    [PyCompOp (l op rights) (local [(define c (desugar-compop l op rights global? env))]
                              (begin ;(display c) (display "\n")
                                     c))]


    [PyLam (args body)
           (local [(define rbody (rec-desugar body global? env))]
                  (DResult 
                    (CFunc args (none)
                         (CReturn                   
                           (DResult-expr rbody)))
                    (DResult-env rbody)))]
    
    [PyFunc (name args body)
            (local [(define body-r (desugar-local-body body args env))]
             (DResult
               (CLet name (CNone)
                     (CAssign (CId name (LocalId))
                              (CFunc args (none) (DResult-expr body-r))))
             (merge-globals env (DResult-env body-r))))]

    [PyFuncVarArg (name args sarg body)
                  (local [(define body-r (desugar-local-body body (append args (list sarg)) env))]
                    (DResult
                      (CLet name (CNone)
                            (CAssign (CId name (LocalId))
                                     (CFunc args (some sarg) (DResult-expr body-r))))
                      (merge-globals env (DResult-env body-r))))]

    [PyReturn (value)
              (local [(define value-r (rec-desugar value global? env))]
                     (DResult (CReturn (DResult-expr value-r)) (DResult-env value-r)))]

    [PyDict (keys values) (local [(define-values (keys-r mid-env)
                                    (map-desugar keys global? env))
                                  (define-values (values-r last-env)
                                    (map-desugar values global? mid-env))]
                            (DResult
                              (CDict (lists->hash keys-r values-r))
                              last-env))]

    [PySet (elts) (local [(define-values (results last-env)
                            (map-desugar elts global? env))]
                    (DResult
                      (CSet results)
                      last-env))]

    [PyList (values) (local [(define-values (results last-env)
                               (map-desugar values global? env))]
                       (DResult
                         (CList results)
                         last-env))]

    [PyTuple (values) (local [(define-values (results last-env)
                                (map-desugar values global? env))]
                        (DResult
                          (CTuple results)
                          last-env))]

    [PySubscript (left ctx slice)
                 (cond
                  [(symbol=? ctx 'Load)
                   (local [(define left-id (new-id))
                           (define left-r (rec-desugar left global? env))]
                    (if (PySlice? slice)
                       (local [(define slice-low (rec-desugar (PySlice-lower slice) global? env))
                               (define slice-up (rec-desugar (PySlice-upper slice) global? env))
                               (define slice-step (rec-desugar (PySlice-step slice) global? env))]
                              (DResult
                                (CLet left-id
                                      (DResult-expr left-r)
                                      (CApp (CGetField (CId left-id (LocalId))
                                                       '__slice__)
                                            (list 
                                                  (CId left-id (LocalId))
                                                  (DResult-expr slice-low)
                                                  (DResult-expr slice-up)
                                                  (DResult-expr slice-step))
                                            (none)))
                                (DResult-env slice-step)))
                       (local [(define slice-r (rec-desugar slice global? (DResult-env left-r)))] 
                              (DResult 
                                (CLet left-id 
                                      (DResult-expr left-r)
                                      (CApp (CGetField (CId left-id (LocalId))
                                                       '__attr__)
                                            (list (CId left-id (LocalId)) (DResult-expr slice-r))
                                            (none)))
                                (DResult-env slice-r)))))]
                  [(symbol=? ctx 'Store)
                   (error 'desugar "bad syntax: PySubscript has context 'Store' outside a PyAssign")]
                  [else (error 'desugar "unrecognized context in PySubscript")])]

    [PyBreak () (DResult (CBreak) env)]

    [PyApp (fun args)
           (local [(define f (rec-desugar fun global? env))
                   (define-values (results last-env)
                     (map-desugar args global? (DResult-env f)))]
             (begin
               ;(display args) (display "\n")
               ;(display results) (display "\n\n")
               (DResult
               (if (CGetField? (DResult-expr f))
                 (local [(define o (CGetField-value (DResult-expr f)))]
                   (CApp (DResult-expr f) (cons o results) (none)))
                 (CApp (DResult-expr f) results (none)))
               last-env)))]

    [PyAppStarArg (fun args sarg)
           (local [(define f (rec-desugar fun global? env))
                   (define-values (results mid-env)
                     (map-desugar args global? (DResult-env f)))
                   (define s (rec-desugar sarg global? mid-env))]
             (DResult
               (if (CGetField? (DResult-expr f))
                 (local [(define o (CGetField-value (DResult-expr f)))]
                   (CApp (DResult-expr f) (cons o results) (some (DResult-expr s))))
                 (CApp (DResult-expr f) results (some (DResult-expr s))))
               (DResult-env s)))]
    
    [PyClass (name bases body)
             (local [(define body-r (rec-desugar body false (extract-globals env)))]
               (DResult
                 (CAssign (CId name (LocalId))
                          (CClass name
                                  (if (empty? bases) 
                                    'object
                                    (first bases))  
                                  (DResult-expr body-r)))
                 (DResult-env body-r)))]
    
    [PyDotField (value attr)
                (local [(define value-r (rec-desugar value global? env))]
                   (DResult
                     (CGetField (DResult-expr value-r) attr)
                     (DResult-env value-r)))]
    
    [PyTryExceptElseFinally (try excepts orelse finally)
                (local [(define try-r (rec-desugar try global? env))
                        (define-values (excepts-r mid-env)
                          (map-desugar excepts global? (DResult-env try-r)))
                        (define orelse-r (rec-desugar orelse global? mid-env))
                        (define finally-r (rec-desugar finally global? (DResult-env orelse-r)))]
                  (DResult
                    (CTryExceptElseFinally 
                      (DResult-expr try-r)
                      excepts-r
                      (DResult-expr orelse-r)
                      (DResult-expr finally-r))
                    (DResult-env finally-r)))]
    
    [PyExcept (types body)
              (local [(define-values (types-r mid-env)
                        (map-desugar types global? env))
                      (define body-r (rec-desugar body global? mid-env))]
                (DResult
                  (CExcept types-r 
                           (none)
                           (DResult-expr body-r))
                  (DResult-env body-r)))]
    
    [PyWhile (test body orelse)
             (local [(define test-r (rec-desugar test global? env))
                     (define body-r (rec-desugar body global? (DResult-env test-r)))
                     (define orelse-r (rec-desugar orelse global? (DResult-env body-r)))]
             (DResult 
                 (CWhile (DResult-expr test-r)
                         (DResult-expr body-r)
                         (DResult-expr orelse-r))
                 (DResult-env orelse-r)))]

    [PyExceptAs (types name body)
                (local [(define-values (types-r mid-env)
                          (map-desugar types global? env))
                        (define body-r (rec-desugar body global? mid-env))]
                  (DResult
                    (CExcept types-r
                             (some name)
                             (DResult-expr body-r))
                    (DResult-env body-r)))]

    [PyAugAssign (op target value)
                 (local [(define target-r (rec-desugar target global? env))
                         (define aug-r (rec-desugar (PyBinOp target op value)
                                                       global? (DResult-env target-r)))]
                 (DResult
                   (CAssign (DResult-expr target-r)
                          (DResult-expr aug-r))
                 (DResult-env aug-r)))]
    ; XXX: target is interpreted twice, independently.
    ; Is there any case where this might cause problems?

    [PyDelete (targets)
              (let ([target (first targets)]) ; TODO: handle deletion of more than one target
                (type-case PyExpr target
                           [PySubscript (left ctx slice)
                                        (letrec ([desugared-target (rec-desugar left global? env)]
                                                 [desugared-slice (rec-desugar slice global? (DResult-env desugared-target))]
                                                 [target-id (new-id)])
                                          (DResult
                                              (CLet target-id (DResult-expr desugared-target)
                                                (CApp (CGetField (CId target-id (LocalId)) '__delitem__)
                                                      (list (CId target-id (LocalId))
                                                            (DResult-expr desugared-slice))
                                                      (none)))
                                              (DResult-env desugared-slice)))]
                           [else (error 'desugar "We don't know how to delete identifiers yet.")]))]

)))

(define (desugar [expr : PyExpr]) : CExpr
  (type-case DesugarResult (rec-desugar expr true (hash empty))
    [DResult (expr env) expr]))

(define-type DesugarResult
   [DResult (expr : CExpr) (env : IdEnv)])

(define (extract-globals [env : IdEnv]) env)

(define (merge-globals [complete-env : IdEnv] [only-globals : IdEnv]) complete-env)

(define (add-ids [ids : (listof symbol)] [type : IdType] [env : IdEnv]) : IdEnv env)
