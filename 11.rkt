#lang racket
(require "util.rkt")
(require memo)

(define day "11")
(define ex1 "aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out")
(define ex2 "svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out")

(define (parse s)
  "Return an adjacency list"
  (let ([D (make-hash)])
    (for ([ln (lines s)])
      (let ([tmp (string-split (string-replace ln ":" ""))])
        (hash-set! D (first tmp) (cdr tmp))))
    D))

(define (part1 s)
  (let ([adj (parse s)])
    (let loop ([node "you"])
      (if (equal? (hash-ref adj node) (list "out"))
          1
          (for/sum ([n (hash-ref adj node)])
            (loop n))))))

(define (part2 s)
  (let ([adj (parse s)])
    
    (define/memoize (solve node dac? fft?)
      (if (equal? (hash-ref adj node) (list "out"))
          (if (and dac? fft?) 1 0)
          (for/sum ([n (hash-ref adj node)])
            (solve n
                   (if (string=? n "dac") #t dac?)
                   (if (string=? n "fft") #t fft?)))))

    (solve "svr" #f #f)))

(time
 (let ([input (file->string (format "~a.txt" day))])
   (printf "Part 1: ~a\n"
           (list (part1 ex1)
                 (part1 input)
                 ))
   (printf "Part 2: ~a\n"
           (list (part2 ex2)
                 (part2 input)
                 ))
   ))
