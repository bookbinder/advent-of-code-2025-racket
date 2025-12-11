#lang racket
(require "util.rkt")


(define day "10")
(define ex1 "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
")

(define (list->num L)
  "When `on` digits are in the positions indicated in L, what is
the final number in base 10?"
  (let loop ([L L] [i 0])
    (if (null? L)
        0
        (if (= (first L) i)
            (+ (expt 2 i) (loop (cdr L) (add1 i)))
            (loop L (add1 i))))))

(define (make-subsets L)
  "Return lists containing length of subset and bitwise-xor of subset.
Sort by shortest length."
  (let ([res '()])
    (let loop ([L (map list->num L)] [path '()])
      (if (null? L)
          (unless (null? path)
            (set! res (cons (list (length path)
                                  (apply bitwise-xor path))
                            res)))
          (begin
            (loop (cdr L) path)
            (loop (cdr L) (cons (first L) path)))))
    (sort res (λ (a b) (< (first a) (first b))))))

(define (parse-line ln)
  "Parse a record into lights, wiring, and joltage."
  (match-let ([(list _ a b c)
               (regexp-match #px"\\[([.#]+)\\] (.*) (.*)" ln)])
    (list (for/sum ([ch a]
                    [pwr (in-naturals)])
            (if (char=? ch #\#) (expt 2 pwr) 0))
          (map ints (string-split b "("))
          (ints c))))

(define (parse s)
  (map parse-line (lines s)))

(define (machine-score target subs)
  "The length of the first subset (xor'd) that equals the target."
  (for/first ([m subs]
              #:when (= target (second m)))
    (first m)))

(define (part1 s)
  (let ([machines (parse s)])
    (for/sum ([m machines])
      (let ([subs (make-subsets (second m))])
        (machine-score (first m) subs)))))

(define (part2 s)
  0)

(time
 (let ([input (file->string (format "~a.txt" day))])
   (printf "Part 1: ~a\n"
           (list (part1 ex1)
                 (part1 input)
                 ))
   ;; (printf "Part 2: ~a\n"
   ;;         (list (part2 ex1)
   ;;               (part2 input)
   ;;               ))
   ))
