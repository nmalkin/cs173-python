#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt")

(define (desugar-boolop [op : symbol] [values : (listof PyExpr)]) : CExpr
  (local [(define first-val (desugar (first values)))]
         (if (> (length values) 1)
          (case op
            ['And (CIf first-val
                       (desugar-boolop op (rest values))
                       first-val)]
            ['Or (CIf first-val
                      first-val
                      (desugar-boolop op (rest values)))])

          (desugar (first values)))))

(define (desugar-compop [l : PyExpr] 
                        [ops : (listof symbol)] 
                        [comps : (listof PyExpr)]) : CExpr
  (local [(define first-right (desugar (first comps)))
          (define l-expr (desugar l))
          (define first-comp (CPrim2 (first ops) l-expr first-right))]
         ;; todo: error here if lengths of ops and comps don't match
         (if (> (length comps) 1)
           (CPrim2 'And
                   first-comp
                   (desugar-compop l (rest ops) (rest comps)))

           first-comp)))

(define (desugar expr)
  (type-case PyExpr expr
    [PySeq (es) (foldl (lambda (e1 e2) (CSeq e2 (desugar e1))) (desugar (first es)) (rest es))]
    [PyAssign (targets value) (foldl (lambda (t asgns)
                                       (CSeq asgns (CAssign (desugar t)
                                                            (desugar value))))
                                       (CAssign (desugar (first targets))
                                                (desugar value))
                                       (rest targets))]

    [PyNum (n) (CNum n)]
    [PyBool (b) (if b (CTrue) (CFalse))]
    [PyStr (s) (CStr s)]
    [PyId (x ctx) (CId x)]

    ; for now just desugar raise as error
    ; TODO: implement real exceptions
    [PyRaise (expr) (CError (desugar expr))]

    [PyPass () (CApp (CFunc empty (CNone)) empty)] ;PyPass is an empty lambda

    [PyIf (test body orelse)
          (CIf (desugar test) (desugar body) (desugar orelse))]

    [PyBinOp (left op right)
             (CPrim2 op
                     (desugar left) 
                     (desugar right))]

    [PyUnaryOp (op operand)
               (CPrim1 op
                       (desugar operand))]

    [PyBoolOp (op values) (desugar-boolop op values)]
              
    [PyCompOp (l op rights) (desugar-compop l op rights)]

    [PyLam (args body)
           (CFunc args 
                  (desugar body))]

    [PyApp (fun args)
           (CApp (desugar fun)
                 (map desugar args))]))

