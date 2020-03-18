#lang racket
(require eopl)

;;; Building an Interpreter for the Simple Arithmetic Language

;;; Defining the Abstract Syntax Tree Structure
(define-datatype ast ast?
  [num (n number?)]
  [bool (b boolean?)]
  [plus (left ast?) (right ast?)]
  [minus (left ast?) (right ast?)]
  [mul (left ast?) (right ast?)]
  [div (left ast?) (right ast?)]
  [equality (left ast?) (right ast?)]
  [zero (tr ast?)]
  )

;;;the abstract syntax
;;; concrete syntax
;;; <exp> ::= <num> | ( <op> <num> <num> )
;;; <op> ::= + | - | *

;;;defining the operator list
(define *ops* '(+ Add - Sub * Mul / Div IsZero = Equals))

;;; mapping operation definition to operation name
(define get-cons
  (lambda(x)
    (cond
      [(eq? x 'Add) plus]
      [(eq? x '+) plus]
      [(eq? x 'Sub) minus]
      [(eq? x '-) minus]
      [(eq? x 'Mul) mul]
      [(eq? x '*) mul]
      [(eq? x 'Div) div]
      [(eq? x '/) div]
      [(eq? x '=) equality]
      [(eq? x 'Equals) equality]
      [(eq? x 'IsZero) zero]
      )))

;;; parsing the input to generate the AST
(define parse
  (lambda (x)
    (cond
    [(number? x) (num x)]
    [(boolean? x) (bool x)]
    [(list? x)
     (cond
       [(= (length x) 3)
        (memq (first x) *ops*)
        (let ([ast1 (parse (second x))]
              [ast2 (parse (third x))])
          ((get-cons (first x)) ast1 ast2))]

       [(= (length x) 2)
        (memq (first x) *ops*)
        (let ([ast (parse (second x))])
          ((get-cons (first x)) ast))]

       [(= (length x) 1)
        (cond
          [(number? (car x)) (num (car x))]
          [(boolean? (car x)) (bool (car x))]
          )]
       )]
       
    [else (error 'parse "Invalid Input" x)]
    )))

(define (divide num denom)
  (when (zero? denom)
    (raise 'Division-by-Zero-Error-Encountered))
  (/ num denom))

;;; evaluating the AST
(define eval-lst
  (lambda(a)
    (cases ast a
      [num(n) n]
      [bool(b) b]
      [plus(l r) (+ (eval-lst l) (eval-lst r))]
      [minus(l r) (- (eval-lst l) (eval-lst r))]
      [mul(l r) (* (eval-lst l) (eval-lst r))]
      [div(l r) (floor (divide (eval-lst l) (eval-lst r)))]
      [equality(l r) (= (eval-lst l) (eval-lst r))]
      [zero(t) (= 0 (eval-lst t))]
      )))

;;; takes a surface level expression as input, parses and evaluates the expression
(define eval
  (lambda(x)
    (eval-lst (parse x))))