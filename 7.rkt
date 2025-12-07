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

(define (simul G start)
  "For part 1, count the number of times we hit a splitter."
  (let ([seen (make-seen G)]
        [res 0])
    (set2! seen start #t)
    (let loop ([pt start])
      (when (char=? (get2 G pt) #\^)
        (set! res (add1 res)))
      (for ([n (neis G pt)])
        (unless (get2 seen n)
          (set2! seen n #t)
          (loop n))))
    res))

(define (part1 s)
  (let* ([G (parse s)]
         [start (find-in-G G #\S)])
    (simul G start)))

(define (part2 s)
  (let* ([G (parse s)]
         [start (find-in-G G #\S)])

    (define/memoize (simul2 pt) #:hash hash
      (if (= (first pt) (sub1 (rr G)))
          (if (char=? (get2 G pt) #\^) 2 1)
          (for/sum ([n (neis G pt)])
            (simul2 n))))    

    (simul2 start)))

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
