#lang racket
(require memo)
(require "util.rkt")

(define day "7")
(define ex1 ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
")

(define (parse s)
  (2dlist->2dvector (map string->list (lines s))))

(define (neis G pt)
  "Get neighbors of PT in G, depending on whether PT is a splitter."
  (filter (in-bounds? G)
          (if (char=? (get2 G pt) #\^)
              (list (map + pt '(0 -1)) (map + pt '(0 1)))
              (list (map + pt '(1 0))))))

(define (part1 s)
  "For part 1, count the number of times we hit a splitter."
  (let* ([G (parse s)]
         [seen (make-seen G)]
         [res 0])
    (let loop ([pt (find-in-G G #\S)])
      (when (char=? (get2 G pt) #\^)
        (set! res (add1 res)))
      (for ([n (neis G pt)])
        (unless (get2 seen n)
          (set2! seen n #t)
          (loop n))))
    res))

(define (part2 s)
  "For part 2, count total possible paths."
  (let* ([G (parse s)])
    (define/memoize (run pt) #:hash hash
      (max (sum (map run (neis G pt)))
           1))
    (run (find-in-G G #\S))))

(time
 (let ([input (file->string (format "~a.txt" day))])
   (printf "Part 1: ~a\n"
           (list (part1 ex1)
                 (part1 input)
                 ))
   (printf "Part 2: ~a\n"
           (list (part2 ex1)
                 (part2 input)
                 ))
   ))
