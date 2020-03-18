#lang racket

;;; Functional Programming on Lists

;;; p15 - returns a list containing n copies of x

(define duple
  (lambda (n x)
    (cond
      [(<= n 0) '()]
      [else (cons x (duple (- n 1) x))])))

;;; p16 - returns a list with each 2-list reversed

(define invert
  (lambda (lst)
    (cond
      [(null? lst) '()]
      [else (cons (list (car (cdr (car lst))) (car (car lst))) (invert (cdr lst)))])))

;;; p17 - wraps parentheses around each top-level element of lst

(define down
  (lambda(lst)
    (cond
      [(null? lst) '()]
      [else (cons (list (car lst)) (down (cdr lst)))])))

;;; p18 - returns a list the same as slist, but with all occurrences of s1 replaced by s2 and all occurrences of s2 replaced by s1

(define swapper
  (lambda(s1 s2 slist)
    (cond
      [(null? slist) '()]
      [(list? (car slist)) (cons (swapper s1 s2 (car slist)) (swapper s1 s2 (cdr slist)))]
      [(equal? s1 (car slist)) (cons s2 (swapper s1 s2 (cdr slist)))]
      [(equal? s2 (car slist)) (cons s1 (swapper s1 s2 (cdr slist)))]
      [else (cons (car slist) (swapper s1 s2 (cdr slist)))])))

;;; p19 - returns a list like lst, except that the n-th element, using zero-based indexing, is x

(define list-set
  (lambda(lst n x)
    (cond
      [(null? lst) '()]
      [(= n 0) (cons x (cdr lst))]
      [else (cons (car lst) (list-set (cdr lst) (- n 1) x))])))

;;; p20 - returns the number of occurrences of s in slist

(define count-occurrences
  (lambda(s slist)
    (cond
      [(null? slist) 0]
      [(list? (car slist)) (+ (count-occurrences s (car slist)) (count-occurrences s (cdr slist)))]
      [(equal? s (car slist)) (+ 1 (count-occurrences s (cdr slist)))]
      [else (count-occurrences s (cdr slist))])))

;;; p21
;;; returns the cartesian product of a symbol with a list

(define mult_line
  (lambda (sym lst)
    (cond
      [(null? lst) '()]
      [else (cons (list sym (car lst)) (mult_line sym (cdr lst)))])))

;;; returns a list of 2-lists that represents the Cartesian product of sos1 and sos2

(define product
  (lambda (sos1 sos2)
    (cond
      [(null? sos1) '()]
      [(null? sos2) '()]
      [else (append (mult_line (car sos1) sos2) (product (cdr sos1) sos2))])))

;;; p22 - returns the list of those elements in lst that satisfy the predicate pred

(define filter-in
  (lambda (pred lst)
    (cond
      [(null? lst) '()]
      [(pred (car lst)) (cons (car lst) (filter-in pred (cdr lst)))]
      [else (filter-in pred (cdr lst))]
      )))

;;; p23 - returns the 0-based position of the first element of lst that satisfies the predicate pred
;;; if no element of lst satisfies the predicate, then list-index returns #f

;;; auxiliary which keeps count of list-index and performs the recursion

(define list-index-count
  (lambda (pred lst count)
    (cond
      [(null? lst) #f]
      [(pred (car lst)) count]
      [else (list-index-count pred (cdr lst) (+ 1 count))])))

;;; calls the auxiliary with base as 0

(define list-index
  (lambda (pred lst)
    (list-index-count pred lst 0)))

;;; p24 - returns #f if any element of lst fails to satisfy pred, and returns #t otherwise

(define every?
  (lambda (pred lst)
    (cond
      [(null? lst) #t]
      [(pred (car lst)) (every? pred (cdr lst))]
      [else #f])))

;;; p25 - returns #t if any element of lst satisfies pred, and returns #f otherwise

(define exists?
  (lambda (pred lst)
    (cond
      [(null? lst) #f]
      [(pred (car lst)) #t]
      [else (exists? pred (cdr lst))])))

;;; p26 - removes a pair of parentheses from each top-level element of lst

(define up
  (lambda (lst)
    (cond
      [(null? lst) '()]
      [(list? (car lst)) (append (car lst) (up (cdr lst)))]
      [else (cons (car lst) (up (cdr lst)))])))

;;; p27 - returns a list of the symbols contained in slist in the order in which they occur when slist is printed
;;; intuitively, flatten removes all the inner parentheses from its argument

(define flatten
  (lambda(slist)
    (cond
      [(null? slist) '()]
      [(list? (car slist)) (append (flatten (car slist)) (flatten (cdr slist)))]
      [else (cons (car slist) (flatten (cdr slist)))]
      )))

;;; p28 - where loi1 and loi2 are lists of integers that are sorted in ascending order
;;; returns a sorted list of all the integers in loi1 and loi2

(define merge
  (lambda (loi1 loi2)
    (cond
      [(null? loi1) loi2]
      [(null? loi2) loi1]
      [(< (car loi1) (car loi2)) (cons (car loi1) (merge (cdr loi1) loi2))]
      [else (cons (car loi2) (merge (cdr loi2) loi1))]
      )))

;;; p29 - returns a list of the elements of loi in ascending order
;;; recursive sort step

(define sort
  (lambda (loi)
    (cond
      [(null? loi) loi]
      [(null? (cdr loi)) loi]
      [else (merge (sort (take loi (floor (/ (length loi) 2)))) (sort (drop loi (floor (/ (length loi) 2)))))]
      )))

;;; p30 - returns a list of elements sorted by the predicate
;;; merge function taking the predicate into account

(define merge/predicate
  (lambda (loi1 loi2 pred)
    (cond
      [(null? loi1) loi2]
      [(null? loi2) loi1]
      [(pred (car loi1) (car loi2)) (cons (car loi1) (merge/predicate (cdr loi1) loi2 pred))]
      [else (cons (car loi2) (merge/predicate (cdr loi2) loi1 pred))]
      )))

;;; recursive sorting

(define sort/predicate
  (lambda (pred loi)
    (cond
      [(null? loi) loi]
      [(null? (cdr loi)) loi]
      [else (merge/predicate (sort/predicate pred (take loi (floor (/ (length loi) 2)))) (sort/predicate pred (drop loi (floor (/ (length loi) 2)))) pred)]
      )))