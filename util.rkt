#lang racket

(provide (all-defined-out))

(define (ints s)
  "If S is a string, extract all integers into a list. A dash before
an int means negative except when the dash is between two integers. Then
it's not part of an integer."
  (if (string? s)
      (map string->number (regexp-match* #px"(?<!\\d)-?\\d+" s))
      s))

(define (+ints s)
  "If S is a string, extract all integers into a list. Disregard
dashes before ints, so no negatives."
  (if (string? s)
      (map string->number (regexp-match* #px"\\d+" s))
      s))

(define (digits s)
  "If S is a string, extract all digits into a list."
  (if (string? s)
      (map string->number (regexp-match* #px"\\d" s))
      s))

(define (digit-chars s)
  "If S is a string, extract all the characters into a list,
but turn numeric chars into ints"
  (for/list ([i (string-length s)])
    (if (char-numeric? (string-ref s i))
        (string->number (string (string-ref s i)))
        (string-ref s i))))

(define (pars s)
  "Parse S into a list of substrings separated by '\n\n'"
  (string-split (string-trim s) #px"\n\n+"))

(define (lines s)
  "Parse S into a list of substrings separated by \n"
  (string-split s "\n"))

(define (pars-lines s)
  "Parse string S into a list of lists, separated first by '\n\n' and then
by '\n'"
  (map lines (pars s)))

(define (alpha-chars s)
  "Return a list of all the alphabetic chars in s"
  (let loop ([i 0])
    (if (= i (string-length s))
        '()
        (if (char-alphabetic? (string-ref s i))
            (cons (string-ref s i) (loop (add1 i)))
            (loop (add1 i))))))

(define (butlast L)
  (if (null? (cdr L))
      '()
      (cons (car L) (butlast (cdr L)))))

(define (sum L) (apply + L))

(define-syntax-rule (push! item L) (begin (set! L (cons item L)) L))
(define-syntax-rule (pop! L) (if (null? L)
                                 (error "Cannot pop from empty list")
                                 (begin0 (car L) (set! L (cdr L)))))
(define-syntax-rule (toggle! x) (begin (if (not x) (set! x #t) (set! x #f)) x))
(define-syntax-rule (inc! x) (begin (set! x (add1 x)) x))
(define-syntax-rule (dec! x) (begin (set! x (sub1 x)) x))

(define (rmap func lst)
  "Recursively map over a list of lists."
  (cond
    ((null? lst) '())
    ((list? (car lst))
     (cons (rmap func (car lst))
           (rmap func (cdr lst))))
    (else (cons (func (car lst))
                (rmap func (cdr lst))))))

(define (ntimes f n)
  "Returns a function that applies f to its argument, n times."
  (λ (x)
    (let loop ([n n] [x x])
      (if (zero? n)
          x
          (loop (sub1 n) (f x))))))

(define (transpose L)
  "Transpose either a list of equal-length strings or lists."
  (cond ((andmap string? L)
         (apply map string (map string->list L)))
        ((andmap list? L)
         (apply map list L))
        (else (error 'transpose-utility
                     "Currently only lists of strings and lists supported"))))


(define (counter L)
  (let ([d (make-hash)])
    (for ([x L])
      (hash-update! d x add1 0))
    d))

(define (2dlist->2dvector L)
  (list->vector
   (map list->vector L)))

(define dirs4 '((-1 0) (0 1) (1 0) (0 -1)))
(define diagonals '((-1 1) (1 1) (1 -1) (-1 -1)))
(match-define (list North East South West) dirs4)
(match-define (list NE SE SW NW) diagonals)
(define dirs8 (append dirs4 diagonals))

;; Getters and setters for 2d and 3d vector-grids
;; Note that the setters return the value that was set
(define (get2 G pt)
  (vector-ref (vector-ref G (first pt)) (second pt)))
(define (set2! G pt val)
  (vector-set! (vector-ref G (first pt)) (second pt) val)
  (vector-ref (vector-ref G (first pt)) (second pt)))
(define (get3 G r c n)
  (vector-ref (vector-ref (vector-ref G r) c) n))
(define (set3! G r c n val)
  (vector-set! (vector-ref (vector-ref G r) c) n val)
  (vector-ref (vector-ref (vector-ref G r) c) n))

(define (in-bounds? G)
  (λ (pt)
    (and (<= 0 (first pt) (sub1 (vector-length G)))
         (<= 0 (second pt) (sub1 (vector-length (vector-ref G 0)))))))

(define (nei4 G pt)
  (let loop ([D dirs4] [res '()])
    (if (null? D)
        res
        (let ([npt (map + pt (car D))])
          (loop (cdr D) (if ((in-bounds? G) npt) (cons npt res) res))))))

(define (nei8 G pt)
  (let loop ([D dirs8] [res '()])
    (if (null? D)
        res
        (let ([npt (map + pt (car D))])
          (loop (cdr D) (if ((in-bounds? G) npt) (cons npt res) res))))))

(define (make-seen G)
  "Given 2d vector G, make a new grid of the same dimensions where
every value is #f. Use it to see which cells have been visited."
  (for/vector ([_ (vector-length G)])
    (make-vector (vector-length (vector-ref G 0)) #f)))

(define (make-seen0 G)
  "Given 2d vector G, make a new grid of the same dimensions where
every value is 0. Use it to see how many times a cell has been visited."
  (for/vector ([_ (vector-length G)])
    (make-vector (vector-length (vector-ref G 0)) 0)))

(define (rr G)
  (vector-length G))

(define (cc G)
  (vector-length (vector-ref G 0)))

(define (pts G)
  (for*/stream ([r (rr G)]
                [c (cc G)])
    (list r c)))

(define (find-in-G G val)
  "Return first pt in G that equal? val"
  (for/first ([pt (in-stream (pts G))]
              #:when (equal? (get2 G pt) val))
    pt))

(define (findall-in-G G val)
  "Return a list all pts in G that equal? val"
  (for/list ([pt (in-stream (pts G))]
             #:when (equal? (get2 G pt) val))
    pt))

(define (findall-in-G-stream G val)
  "Return a stream of all pts in G that equal? val"
  (for/stream ([pt (in-stream (pts G))]
               #:when (equal? (get2 G pt) val))
    pt))

(define (print-vector-grid V)
  "Prints out a vector grid as rows of strings."
  (displayln
   (string-join
    (for/list ([r (vector-length V)])
      (apply string (vector->list (vector-ref V r))))
    "\n"))
  (displayln ""))

;; (define (mybfs G start)
;;   "Skeleton for BFS. This returns how many times a certain character
;; is found. Relies on NEI function to find neighbors, and GET2 and SET2! This
;; will only go 4 levels deep (see break condition). Doesn't need a DEQUE
;; data structure. G is a 2d vector. START is a list (r c) where the BFS begins."
;;   (let ([seen (for/vector ([_ (vector-length G)])
;;                 (make-vector (vector-length (vector-ref G 0)) #f))])
;;     (for/fold ([q (list start)]
;;                [s-count 0]
;;                #:result s-count)
;;               ([i (in-naturals)]
;;                #:break (or (null? q) (= i 5)))
;;       (for/fold ([nq '()]
;;                  [ns s-count])
;;                 ([node q])
;;         (when (char=? (get2 G node) #\S)
;;           (set! ns (add1 ns)))
;;         (for ([n (nei G node)])
;;           (unless (get2 seen n)
;;             (set2! seen n #t)
;;             (set! nq (cons n nq))))
;;         (values nq ns)))))

(define (prefix-sum L)
  (if (null? L)
      0
      (let loop ([L L] [cur 0])
        (if (null? L)
            '()
            (cons (+ cur (car L))
                  (loop (cdr L) (+ cur (car L))))))))

(define (binsearch V target)
  "Searches for target in vector V. If found, returns index."
  (let loop ([l 0] [r (sub1 (vector-length V))])
    (if (>= l r)
        (if (= (vector-ref V l) target)
            l
            #f)
        (let ([mid (floor (/ (+ l r) 2))])
          (if (< (vector-ref V mid) target)
              (loop (add1 mid) r)
              (loop l mid))))))

(define (binsearch-arg low high func target)
  "Searches for target in range low-high. Calls FUNC on idx as a test."
  (let loop ([l low] [r high])
    (if (>= l r)
        (if (= (func l) target) l #f)
        (let ([mid (floor (/ (+ l r) 2))])
          (if (< (func mid) target)
              (loop (add1 mid) r)
              (loop l mid))))))

(define (bisect-left V target)
  "Returns earliest index in vector V where target could be inserted,
maintaining non-descending order."
  (let loop ([l 0] [r (vector-length V)])
    (if (>= l r)
        l
        (let ([mid (floor (/ (+ l r) 2))])
          (if (< (vector-ref V mid) target)
              (loop (add1 mid) r)
              (loop l mid))))))

(define (bisect-right V target)
  "Returns latest index in vector V where target could be inserted,
maintaining non-descending order."
  (let loop ([l 0] [r (vector-length V)])
    (if (>= l r)
        l
        (let ([mid (floor (/ (+ l r) 2))])
          (if (<= (vector-ref V mid) target)
              (loop (add1 mid) r)
              (loop l mid))))))


