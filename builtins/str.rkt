#lang plai-typed

(require "../python-core-syntax.rkt"
         "../util.rkt")
(require (typed-in racket/base (string=? : (string string -> boolean)))
         (typed-in racket/base (string>? : (string string -> boolean)))
         (typed-in racket/base (string<? : (string string -> boolean)))
         (typed-in racket/base (string-length : (string -> number))))

(define str-class : CExpr
  (CClass
   'str 
   'object
   (seq-ops (list ;;how do I write this one?
                  ;(def '__init__
                  ;  (CFunc (list 'self 'inputstr)
                  ;         (CAssign
                  ;           (CId 'self)
                  ;           (CObject
                  ;             'str
                  ;             (some (MetaVal (CId 'inputstr)))))))
                  (def '__add__
                    (CFunc (list 'self 'other)
                           (CReturn (CBuiltinPrim 'str+
                                                  (list
                                                   (CId 'self)
                                                   (CId 'other))))))
                  (def '__mult__
                    (CFunc (list 'self 'other)
                           (CReturn (CBuiltinPrim 'str*
                                         (list
                                          (CId 'self)
                                          (CId 'other))))))
                  (def '__cmp__
                     (CFunc (list 'self 'other)
                            (CReturn (CBuiltinPrim 'strcmp
                                         (list
                                           (CId 'self)
                                           (CId 'other))))))
                  (def '__len__
                     (CFunc (list 'self)
                            (CReturn (CBuiltinPrim 'strlen
                                         (list
                                           (CId 'self))))))))))

(define (make-builtin-str [s : string]) : CExpr
  (CObject
   'str
   (some (MetaStr s))))

(define (str+ (args : (listof CVal))) : (optionof CVal)
  (check-types args 'str 'str
               (some (VObject 'str 
                              (some (MetaStr
                                     (string-append (MetaStr-s mval1)
                                                    (MetaStr-s mval2))))
                              (hash empty)))))

(define (str (args : (listof CVal))) : (optionof CVal)
  (check-types args 'str
               (some (VObject 'str
                              (some (MetaStr (MetaStr-s mval1)))
                              (hash empty)))))

(define (str* (args : (listof CVal))) : (optionof CVal)
  ;(let ([str*l 
  (check-types args 'str 'num
                      (some (VObject 'str
                                     (some (MetaStr 
                                      (str*-rec (MetaStr-s mval1)
                                                (MetaNum-n mval2))))
                                     (hash empty)))))
    #|(if (none? str*l)
      (check-types args 'num 'str
             (some (VObject 'str
                            (some (MetaStr 
                             (str*-rec (MetaStr-s mval2)
                                       (MetaNum-n mval1))))
                            (hash empty))))
      str*l)))|#

(define (str*-rec [str : string] [num : number]) : string
  (cond
    [(<= num 0) ""]
    [else (string-append str (str*-rec str (sub1 num)))]))

(define (strcmp [args : (listof CVal)]) : (optionof CVal)
  (check-types args 'str 'str
     (some (VObject 'num
                    (some (MetaNum
                      (let ([str1 (MetaStr-s mval1)]
                            [str2 (MetaStr-s mval2)])
                        (cond
                          [(string<? str1 str2) -1]
                          [(string>? str1 str2) 1]
                          [(string=? str1 str2) 0]))))
                    (hash empty)))))

(define (strlen [args : (listof CVal)]) : (optionof CVal)
  (check-types args 'str
     (some (VObject 'num
                    (some (MetaNum
                            (string-length (MetaStr-s mval1))))
                    (hash empty)))))
