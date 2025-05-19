#lang pl




;;============================================================> Question 1 <======================================================================

#|
   Solution description:
   First of all, we took the code-named: <AE> interpreter – for your convenience., from the Assignment 3 pages 9-10
   and we modify it.
   1) in the BNF we place the operator at the end of the expration ,we moved the operator after the AE verebels
   for exmplae: we change { + <AE> <AE> } to { <AE> <AE> + }
   we did this change so the BNF will represent currectly the position of the operator
   2) int the define-type AE we add the pow and sqr variants as needed. so we can use these operators in AE
   3) in the parse-sexpr function we moved the operator in the list at the match sxp , to the end of the list
   and also add the match for pow and sqr
   for example: we change (list '+ l r) to (list l r '+)
   we did this change because the string we get as an input will be in a postfix form,, which means the operator will be at the end
   4) in eval we add 2 match options: (Pow l r), and (Sqr op) to deal with the eval of pow and sqr
   in pow: first we add a condition to check if the second argument is an integer , if it, not an integer
   then raise an error, otherwise use the function pow with the two arguments.
   pow function takes the first argument and raises it to the power of the second argument.
   pow use the helper function.
   in sqr: we use the square function, square function takes the argument and multiplies it by himself
 
   solving process:
   In the solving process of this question, we had some difficulty of how to create a tail recursion
   we look back at the code in the practice presentation for tail recursion
   how much time has been invested to solve the question:
   it takes us around 1.5 hours to solve this question
   console other people:
   we did not need to console others for this question
|#




#| BNF for the AE language:
 <AE> ::= <num>
 | {<AE> <AE> + }
 | {<AE> <AE> - }
 | {<AE> <AE> * }
 | {<AE> <AE> / }
 | {<AE> <AE> power }
 | {<AE> <AE> sqr }
|# ;; AE abstract syntax trees
 (define-type AE
 [Num Number]
 [Add AE AE]
 [Sub AE AE]
 [Mul AE AE]
 [Div AE AE]
 [Pow AE AE]
 [Sqr AE])
 (: parse-sexpr : Sexpr -> AE)
 ;; to convert s-expressions into AEs
 (define (parse-sexpr sexpr)
 (match sexpr
 [(number: n) (Num n)]
 [(list lhs rhs '+)
(Add (parse-sexpr lhs) (parse-sexpr rhs))]
 [(list lhs rhs '-)
(Sub (parse-sexpr lhs) (parse-sexpr rhs))]
 [(list lhs rhs '*)
(Mul (parse-sexpr lhs) (parse-sexpr rhs))]
 [(list lhs rhs '/)
(Div (parse-sexpr lhs) (parse-sexpr rhs))]
  [(list lhs rhs 'power)
   (Pow (parse-sexpr lhs) (parse-sexpr rhs))]
  [(list op 'sqr)
   (Sqr (parse-sexpr op))]
 [else
(error 'parse-sexpr "bad syntax in ~s" sexpr)]))


(: parse : String -> AE)
 ;; parses a string containing an AE expression to AE AST
 (define (parse str)
 (parse-sexpr (string->sexpr str)))

(: eval : AE -> Number)
 ;; consumes an AE and computes the corresponding number
 (define (eval expr)
 (cases expr
 [(Num n) n]
 [(Add l r) (+ (eval l) (eval r))]
 [(Sub l r) (- (eval l) (eval r))]
 [(Mul l r) (* (eval l) (eval r))]
 [(Div l r) (/ (eval l) (eval r))]
 [(Pow l r) (if (integer? (eval r)) (pow (eval l) (eval r)) (error 'parse-sexpr "eval: power expects an integer power,
got"))]
 [(Sqr op) (square (eval op))]))


(: run : String -> Number)
;;evaluate an AE program contained in a string
 (define (run str)
 (eval (parse str)))


(: square : Number -> Number)
(define (square num)
  (* num num))


(: pow : Number Number -> Number)
(define (pow l r)
  (helper l r 1))


(: helper : Number Number Number -> Number)
(define (helper l r acc)
  (if (eq? r 0) acc (helper l (- r 1) (* acc l))))


(test (run "3") => 3)
 (test (run "{3 4 +}") => 7)
 (test (run "{{3 4 -} 7 +}") => 6)
 (test (run "{{3 4 power} 7 +}") => 88)
 (test (run "{{2 4 power} {5 sqr} +}") => 41)
(test (run "{{2 4/5 power} {5 sqr} +}")
 =error> "eval: power expects an integer power,
got")
(test (run "{{2 3 *} {4 2 /} power}") => 36)
(test (run "{{2 3 sqrt} {4 2 /} power}") =error> "parse-sexpr: bad syntax in (2 3 sqrt)")






;;=============================================================> Question 2 <==========================================================================
#|
   Solution description:
   Full parser and evaluator for LE grammer :

   first of all, we took the code in 2. a Parser for the LE language, from the Assignment 3 pages 5 and 
   and we modify it.
   1) we add to the define-type LIST: Append (Listof LIST) for the append option, get a list of LIST,
   Cons LE LIST for the cons option, get LE and list, LST  (Listof LE) wich is a regular list, and also a null
   2) we add to the define-type ATOM : NumL Number for a number and SymL Symbol for a symbol
   3) parse-sexpr->LISTs, we add the map parse-sexpr->LIST sexprs so all the s-exprs list arguments will be converted into a list of LISTs
   4) parse-sexprLE , we convert s-expressions into LEs by the right match for the sexpr
   number to NumL, null to Null, and a symbol to SymL.
   for the list option, the match check if the expression starts with the list  then use the list constructor LST 
   for the append option, the match check if the expression starts with append then use the append contractor Append 
   for the cons option, the match check if the expression starts with cons then use the append contractor Cons  
   otherwise, raise an error  "bad syntax "
 
   solving process:
   In the solving process of this question, we had some difficulty understanding how to define the variants and arguments
   and another difficulty with how to write the parser in the right way and use the given skeleton functions
   we dealt with this difficulty by matching all the variants one by one to the right functions
   how much time has been invested to solve the question:
   it takes us around 8 hours to solve this question
   console other people:
   we did not need to console others for this question
|#

;;2.a parser for LE

;; LE abstract syntax trees
(define-type LE = (U LIST ATOM))
;; LIST abstract syntax trees
(define-type LIST
 [Cons LE LIST]
 [Append (Listof LIST)]
 [LST  (Listof LE)]
 [NullL])
;; ATOM abstract syntax trees
(define-type ATOM
 [NumL Number]
 [SymL Symbol])





(: parse-sexpr->LEs : (Listof Sexpr) -> (Listof LE))
 ;; converts a list of s-expressions into a list of LEs
 (define (parse-sexpr->LEs sexprs)
 (map parse-sexprLE sexprs))

(: parse-sexpr->LISTs : (Listof Sexpr) -> (Listof
LIST))
;; converts a list of s-exprs into a list of LISTs
 (define (parse-sexpr->LISTs sexprs)
 (map parse-sexpr->LIST sexprs))

(: parse-sexpr->LIST : Sexpr -> LIST)
 (define (parse-sexpr->LIST sexpr)
 (let ([ast (parse-sexprLE sexpr)])
 (if (LIST? ast) ast (error 'parsesexprLE "expected LIST; got"))))


(: parse-sexprLE : Sexpr -> LE)
;; to convert s-expressions into LEs
(define (parse-sexprLE sexpr)
 (match sexpr
 [(number: n) (NumL n)]
 ['null (NullL)]
 [(symbol: s) (SymL s)]
 [(cons 'list lst) (LST (parse-sexpr->LEs lst))]
 [(cons 'append lst) (Append (parse-sexpr->LISTs lst))]
 [(list 'cons l r) (Cons (parse-sexprLE l)(parse-sexpr->LIST r))]
 [else (error 'parsesexprLE "bad syntax in ~s" sexpr)]))
       

(: parseLE : String -> LE)
 ;; parses a string containing a LE expression to a
 ;; LE AST
(define (parseLE str)
 (parse-sexprLE (string->sexpr str))) 



;;Tests for Parse:

(test (parseLE "null") => (NullL)) 
(test (parseLE "12") => (NumL 12)) 
(test (parseLE "boo") => (SymL 'boo))

(test (parseLE "{list}") => (LST (list)))
(test (parseLE "{list 1 2 3}") => (LST (list (NumL 1) (NumL 2) (NumL 3))))
(test (parseLE "{list 1 2 null}") => (LST (list (NumL 1) (NumL 2) (NullL))))

(test (parseLE "{append}") => (Append (list)))
(test (parseLE "{append {list 1}}") => (Append (list (LST (list (NumL 1))))))
(test (parseLE "{append {list 1} {list 2} {list 3}}") => (Append (list (LST (list (NumL 1))) (LST (list (NumL 2))) (LST (list (NumL 3))))))


(test (parseLE "{cons 1 null}") => (Cons (NumL 1) (NullL)))
(test (parseLE "{cons 1 {list}}") => (Cons (NumL 1) (LST (list))))
(test (parseLE "{cons 1 {list 2 3}}") => (Cons (NumL 1) (LST (list (NumL 2) (NumL 3)))))




;;2.b - eval for LE
(: eval-append-args : (Listof LE) -> (Listof (Listof Any))) 
 ;; evaluates LE expressions by reducing them to lists 
 (define (eval-append-args exprs) 
 (if (null? exprs) null 
 (let ([fst-val (evalLE (first exprs))]) 
 (if (list? fst-val) 
 (cons fst-val (eval-append-args (rest exprs))) 
 (error 'evalLE "append argument: expected 
 List got ~s" fst-val)))))


 (: evalLE : LE -> Any) 
 ;; evaluates LE expressions by reducing them to numbers 
 (define (evalLE expr) 
 (if (LIST? expr) 
 (cases expr
 [(NullL) null]  
 [(LST lst) (map evalLE lst)] 
 [(Cons l r)
  (let ([eval_r (evalLE r)])
    (if (list? eval_r)
         (cons (evalLE l) eval_r)
         (error 'evalLE "append argument: expected List got ~s" eval_r)))]
 [(Append lst) (apply append (eval-append-args lst))]) 
 (cases expr 
 [(NumL n) n] 
 [(SymL s) s]))) 
 
 (: runLE : String -> Any) 
 ;; evaluate a WAE program contained in a string 
 (define (runLE str) 
 (evalLE (parseLE str)))





;;Tests for Eval:
;;Numbers & Symbols :
(test (runLE "null") => null) 
(test (runLE "12") => 12) 
(test (runLE "boo") => 'boo)

;;Lists :
(test (runLE "{list}") => '())
(test (runLE "{list 1 boo}") => '(1 boo))
(test (runLE "{list 1 2 null}") => '(1 2 ()))
(test (runLE "{list 1 poo {list 2 3 {list 5 {cons 1 null}}}}") => '(1 poo (2 3 (5 (1)))))
(test (runLE "{cons 1 {append {list 2 3} {cons 4 {cons 5 null}}}}") => '(1 2 3 4 5))


;;Cons :
(test (runLE "{cons}") =error> "parsesexprLE: bad syntax in (cons)")
(test (runLE "{cons 1 null}") => '(1))
(test (runLE "{cons 1 {cons 2 null}}") => '(1 2))
(test (runLE "{cons 1 {cons 2 {cons 3 {list 4 5}}}}") => '(1 2 3 4 5))
(test (runLE "{list {cons 2 1}}") =error> 
"parsesexprLE: expected LIST; got")


;;Append :
(test (runLE "{append}")=>'())
(test (runLE "{append {list 1} {list 2}}")=> '(1 2))
(test (runLE "{append {list 1} 2}")=error>"parsesexprLE: expected LIST; got")
(test (runLE "{append {list 1} {list 2} {cons 3 null} {append {cons 4 {cons 5 null}}}}")=>'(1 2 3 4 5))



(test (runLE "{cons 1 {cons two null}}") => 
'(1 two)) 
(test (runLE "{list 1 2 3}") => '(1 2 3))  








