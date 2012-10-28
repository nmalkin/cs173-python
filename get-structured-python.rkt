#lang plai

(require "python-syntax.rkt")
(require "parse-python.rkt")
(require racket/match
         racket/list)

#|

Python parses as a JSON structure that we export from Python's ast
module.  You should use this file to turn it into a plai-typed data
structure that you define in python-syntax.rkt

|#

;; given a node in json form produce a symbol corresponding to its nodename
(define (nodetype->symbol nodejson)
  (string->symbol (hash-ref nodejson 'nodetype)))

(define (get-structured-python pyjson)
  (match pyjson
    [(hash-table ('nodetype "Module") ('body expr-list))
     (PySeq (map get-structured-python expr-list))]


    [(hash-table ('nodetype "Expr") ('value expr))
     (get-structured-python expr)]

    [(hash-table ('nodetype "Call")
                 ('keywords keywords) ;; ignoring keywords for now
                 ('kwargs kwargs)     ;; ignoring kwargs for now
                 ('starargs starargs) ;; ignoring starargs for now
                 ('args args-list)
                 ('func func-expr))
     (PyApp (get-structured-python func-expr)
            (map get-structured-python args-list))]

    [(hash-table ('nodetype "BinOp")
                 ('left l)
                 ('op op)
                 ('right r))
     (PyBinOp (get-structured-python l)
              (nodetype->symbol op)
              (get-structured-python r))]

    [(hash-table ('nodetype "UnaryOp")
                 ('op op)
                 ('operand operand))
     (PyUnaryOp (nodetype->symbol op)
                (get-structured-python operand))]

    [(hash-table ('nodetype "Compare")
                 ('left l)
                 ('ops ops)
                 ('comparators c))
     (PyCompOp (get-structured-python l)
               (map nodetype->symbol ops)
               (map get-structured-python c))]
    [(hash-table ('nodetype "BoolOp")
                 ('values values)
                 ('op op))
     (PyBoolOp (nodetype->symbol op) (map get-structured-python values))]

    [(hash-table ('nodetype "Name")
                 ('ctx ctx)        
                 ('id id))
     (PyId (string->symbol id) (nodetype->symbol ctx))]

    [(hash-table ('nodetype "Num")
                 ('n n))
     (PyNum n)]

    [(hash-table ('nodetype "Pass"))
     (PyPass)]

    [(hash-table ('nodetype "Str")
                 ('s str-value))
     (PyStr str-value)]

    [(hash-table ('nodetype "Lambda")
                 ('args args)
                 ('body body))
     (PyLam (get-structured-python args)
            (get-structured-python body))]

    [(hash-table ('nodetype "arguments")
                 ('args arg-list))
     (map get-structured-python arg-list)]

    [(hash-table ('nodetype "arg")
                 ('arg name)) 
     (string->symbol name)]

    [(hash-table ('nodetype "Assign")
                 ('value value)
                 ('targets targets))

     (PyAssign (map get-structured-python targets)
               (get-structured-python value))]

    [(hash-table ('nodetype "Raise") 
                 ('exc exc)
                 ('cause c))
     (PyRaise (get-structured-python exc))]


    [(hash-table ('nodetype "If")
                 ('body body)
                 ('test test)
                 ('orelse orelse))
     (PyIf (get-structured-python test)
           (PySeq
             (map get-structured-python body))
           (get-structured-python orelse))]

    [empty (PyPass)]

    [_ (error 'parse "Haven't handled a case yet")]))


;; tests!
(print-only-errors true)

(define test-python-path "/usr/local/bin/python3.2")
(test (get-structured-python (parse-python/string "x = 5" test-python-path))
      (PySeq
        (list (PyAssign (list (PyId 'x 'Store))
                (PyNum 5)))))

(test (get-structured-python (parse-python/string "pass" test-python-path))
      (PySeq
        (list (PyPass))))

(test (get-structured-python (parse-python/string "\"string\"" test-python-path))
      (PySeq
        (list (PyStr "string"))))

(test (get-structured-python (parse-python/string "raise 5" test-python-path))
      (PySeq
        (list (PyRaise (PyNum 5)))))

(test (get-structured-python (parse-python/string "f(x)" test-python-path))
      (PySeq 
        (list (PyApp (PyId 'f 'Load) (list (PyId 'x 'Load))))))
(test (get-structured-python (parse-python/string "2 + 1" test-python-path))
      (PySeq
        (list (PyBinOp (PyNum 2) 'Add (PyNum 1)))))

(test (get-structured-python (parse-python/string "1 > 2" test-python-path))
      (PySeq
        (list (PyCompOp (PyNum 1)
                        (list 'Gt)
                        (list (PyNum 2))))))

(test (get-structured-python (parse-python/string "if True: 5"
                                                  test-python-path))
      (PySeq
        (list (PyIf (PyId 'True 'Load)
                    (PySeq (list (PyNum 5)))
                    (PyPass)))))
