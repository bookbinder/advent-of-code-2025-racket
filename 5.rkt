#lang racket
(require "util.rkt")

(define day "5")
(define ex1 "3-5
10-14
16-20
12-18

1
5
8
11
17
32")

(define (parse s)
  (let* ([ns (pars s)]
         [rngs (map ints (lines (first ns)))]
         [ids (ints (second ns))])
    (values rngs ids)))

(define (in-a-range rngs)
  "Returns a function that checks whether its argument is in RNGS."
  (λ (id)
    (ormap (λ (x) (<= (first x) id (second x))) rngs)))

(define (combine-ranges rngs)
  "Given a list of ranges, return a new list that sorts them
and combines any overlapping or contiguous ranges."
  (let ([rngs (sort rngs (λ (a b) (< (first a) (first b))))])
    (let loop ([L (cdr rngs)] [saved (first rngs)])
      (if (null? L)
          (list saved)
          (let ([r1 saved]
                [r2 (first L)])
            (if (<= (first r2) (add1 (second r1)))
                (loop (cdr L)
                      (list (first r1) (max (second r2) (second r1))))
                (cons saved (loop (cdr L) r2))))))))

(define (part1 in)
  (let-values ([(rngs ids) (parse in)])
    (count (in-a-range rngs) ids)))

(define (part2 in)
  (let-values ([(rngs _) (parse in)])
    (for/sum ([r (combine-ranges rngs)])
      (add1 (- (second r) (first r))))))

(time
 (let ([input (file->string (format "~a.txt" day))])
   (printf "Part 1: ~a\n"
           (list
            (part1 ex1)
            (part1 input)
            ))
   (printf "Part 2: ~a\n"
           (list
            (part2 ex1)
            (part2 input)
            ))
   ))
