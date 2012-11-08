#lang plai-typed

(require "../python-core-syntax.rkt")
(require "../util.rkt")

(define str-class : CExpr
	(CClass
		'str 
		'object
		(seq-ops
			(list
				(def '__add__
						 (CFunc (list 'self 'other)
										(CBuiltinPrim 'str+
																	(list
																		(CId 'self)
																		(CId 'other)))))
				(def '__mult__
						 (CFunc (list 'self 'other)
										(CBuiltinPrim 'str*
																	(list
																		(CId 'self)
																		(CId 'other)))))
				(def '__rmult__
						 (CFunc (list 'self 'other)
										(CBuiltinPrim 'str*
																	(list
																	(CId 'self)
																	(CId 'other)))))))))

(define (make-builtin-str [s : string]) : CExpr
	(CObject
		'str
		(MetaStr s)
		(make-hash empty)))

#|(define (str+ [self-str : CVal] [other : CVal])
	(type-case CVal self-str
		[VObject (ante-name mval dict)
						 (cond
						 	[(symbol=? ante-name 'str)
							 (type-case CVal other
								[VObject (o-ante-name o-mval o-dict)
												 (cond
													 [(symbol=? o-ante-name 'str)
														(let ([s1 (MetaStr-s (some-v mval))]
																	[s2 (MetaStr-s (some-v o-mval))])
															(string-append s1 s2))])]
								[else (error 'str+ "Second argument not a string")])])]
		[else (error 'str+ "First argument not a string")]))|#
