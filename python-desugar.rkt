#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt"
         "util.rkt"
         "builtins/num.rkt" 
         "builtins/str.rkt")

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
                        [comparators : (listof PyExpr)]) : CExpr
  (local [(define first-right (desugar (first comparators)))
          (define l-expr (desugar l))
          (define first-comp (desugar (PyBinOp  l (first ops) (first comparators))))]
         (if (> (length comparators) 1) 
           (CIf first-comp
                (desugar-compop (first comparators) (rest ops) (rest comparators))
                first-comp)
           first-comp)))

(define (desugar [expr : PyExpr]) : CExpr
  (type-case PyExpr expr
    [PySeq (es) (foldl (lambda (e1 e2) (CSeq e2 (desugar e1))) (desugar (first es)) (rest es))]
    [PyAssign (targets value) (foldl (lambda (t asgns)
                                       (CSeq asgns (CAssign (desugar t)
                                                            (desugar value))))
                                       (CAssign (desugar (first targets))
                                                (desugar value))
                                       (rest targets))]
    [PyNum (n) (make-builtin-num n)]
    [PyBool (b) (if b (CTrue) (CFalse))]
    [PyStr (s) (make-builtin-str s)]
    [PyId (x ctx) (CId x)]

    ; for now just desugar raise as error
    ; TODO: implement real exceptions
    [PyRaise (expr) (CError (desugar expr))]

    [PyPass () (CApp (CFunc empty (CNone)) empty)] ;PyPass is an empty lambda

    [PyIf (test body orelse)
          (CIf (desugar test) (desugar body) (desugar orelse))]

    [PyBinOp (left op right)
             (let ([left-c (desugar left)]
                   [right-c (desugar right)]) 
               (case op 
                 ['Add (CApp (CGetField left-c '__add__) 
                             (list left-c right-c))] 
                 ['Eq (CApp (CGetField left-c '__eq__)
                            (list left-c right-c))]
                 ['Gt (CApp (CGetField left-c '__gt__)
                            (list left-c right-c))]
                 ['Lt (CApp (CGetField left-c '__lt__)
                            (list left-c right-c))]
                 ['NotEq (desugar (PyUnaryOp 'Not (PyBinOp left 'Eq right)))]

                 ['LtE (desugar (PyBoolOp 'Or
                                          (list (PyBinOp left 'Eq right)
                                                (PyBinOp left 'Lt right))))]
                 ['GtE (desugar (PyBoolOp 'Or
                                          (list (PyBinOp left 'Eq right)
                                                (PyBinOp left 'rt right))))]
                 [else (CPrim2 op (desugar left) (desugar right))]))]

    [PyUnaryOp (op operand)
               (CPrim1 op
                       (desugar operand))]

    [PyBoolOp (op values) (desugar-boolop op values)]
              
    [PyCompOp (l op rights) (desugar-compop l op rights)]

    [PyLam (args body)
           (CFunc args 
                  (CReturn                   
                   (desugar body)))]
    
    [PyFunc (name args body)
            (CAssign (CId name)
                     (CFunc args
                            (desugar body)))]
    
    [PyReturn (value)
              (CReturn (desugar value))]

    [PyDict (keys values)
            (CDict (lists->hash (map desugar keys)
                                (map desugar values)))]

    [PyApp (fun args)
           (CApp (desugar fun)
                 (map desugar args))]
    
    [PyClass (name bases body)
             (CAssign (CId name)
                      (CClass name
                              (if (empty? bases) 
                                'object
                                (first bases))  
                              (desugar body)))]
    
    [PyDotField (value attr)
                (CGetField (desugar value)
                           attr)]))
