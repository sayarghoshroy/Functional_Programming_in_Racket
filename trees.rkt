#lang racket

;;; Functional Programming on Trees

(require eopl)

(define-datatype tree tree?
	[leaf (key number?)]
	[node (key number?) (left-parent tree?) (right-parent tree?)])

;;; computes the reduced value of the tree based on the binary function f

(define reduce_run
  (lambda (f tr)
    (cases tree tr
      (leaf(v) v)
      (node (main left right) (f main (f (reduce_run f right) (reduce_run f left))))
      )))

;;; provides the initial value for the reduction

(define treeduce
  (lambda (f init tr)
    (f init (reduce_run f tr))))

;;; applies the function f to every element in the tree

(define tree/map
  (lambda (f tr)
    (cases tree tr
      (leaf(v) (leaf (f v)))
      (node(main left right) (node (f main) (tree/map f left) (tree/map f right)))
      )))

;;; shows a series of left, right steps to reach the goal state 'n'

(define path
  (lambda (n tr)
    (cases tree tr
      (leaf(v) '())
      (node (main left right)
            (cond
              [(= main n) '()]
              [(< main n) (cons 'right (path n right))]
              [else (cons 'left (path n left))])))))

;;; reduction recursion

(define reduction
  (lambda (f lst)
    (cond
      [(null? lst) '()]
      [(null? (cdr lst)) (car lst)]
      [else (f (reduction f (cdr lst)) (car lst))])))

;;; the reduce function - reduces a list

(define reduce
  (lambda (f init lst)
    (f init (reduction f lst))))

;;; defining a new join operation for proper reversal of lists
;;; NOTE: using cons instead of connect will not give ideal results

(define connect
  (lambda (a b)
    (cond
      [(list? a)
       (cond
         [(list? b) (append a b)]
         [else (append a (list b))])]
      [else
       (cond
         [(list? b) (append (list a) b)]
         [else (append (list a) (list b))])]
      )))
       

;;; uses the reduce function defined above
;;; reverses the list 'lst'

(define reverse
  (lambda (lst)
    (reduce connect '() lst)))

(define number-elements
  (lambda (lst)
    (if (null? lst)
        '()
        (g (list 0 (car lst))
           (number-elements (cdr lst))))))

;;; auxiliary for g, increments element tags by 1

(define do_all
  (lambda (lst)
    (if (null? lst) '()
    (cons (cons (+ 1 (caar lst)) (cdar lst)) (do_all (cdr lst)))
    )))

;;; the required function 'g'

(define g
  (lambda (lst1 lst2)
    (append (list lst1) (do_all lst2))
    ))

;;; swaps out the 2nd element from the beginning

(define swap
  (lambda (lst)
    (cons (car lst) (cddr lst))))

;;; does the bubble swap operation on consecutive array elements

(define bubble
  (lambda (n lst)
    (cond
      [(null? lst) '()]
      [(= n 0)(cdr lst)]
      [(null? (cdr lst)) (list (car lst))]
      [(> (car lst) (cadr lst)) (cons (cadr lst) (bubble (- n 1) (swap lst)))]
      [else (cons (car lst) (bubble (- n 1) (cdr lst)))]
      )))

;;; repeats the bubble opertions till array is sorted

(define repeat
  (lambda (n arr_len lst)
    (cond
      [(< n 2) lst]
      [else (repeat (- n 1) arr_len (bubble arr_len lst))]
      )))

;;; sorts the array lst in ascending order

(define bubble-sort
  (lambda (lst)
    (repeat (length lst) (length lst) lst)))
