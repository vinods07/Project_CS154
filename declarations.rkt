#lang racket

(provide int line-wrap)
(provide char )
(provide XOR )
(provide index-of )
(provide at-index)
(provide zip)
(provide bin->dec)
(provide dec->bin)
(provide alphabet?)
(provide present?)
(provide dec->hex)
(provide hex->dec)
(provide combine-f sort-by-freq)
(provide sublist drop-n)
(provide ideal-msg)
(provide singleton?)
(provide ideal-matrix)

(define chars '(#\' #\space #\newline #\, #\. #\? ))

(define (singleton? l)
  (= 1 (length l)))

(define ideal-msg (string-upcase "True love is possibly the most fulfilling of life's secret treasures. but love by a lesser standard is still extremely important for the human experience. In the poem True Love by Wislawa Szymborska Wislawa talks of how true love is overrated and unnecessary. But in truth the argument against true love is created to comfort those who lack it. Love, if not true love is an crucial emotion for the human race; it is important for psychological development, social development, and in the end happiness."))

(struct matrix (rows) #:transparent)

(define (dec->bin n)
  (if (= n 0) 0
      (if  (odd? n) (+ 1 (* 10 (dec->bin (quotient n 2))))
           (* 10 (dec->bin (quotient n 2))))))

(define (alphabet? c)
  (if (char? c)
      (alphabet? (int c))
      (and (<= c 90) (>= c 65))))

(define (present? x l)
  (if (member x l) #t #f))

(define (bin->dec b)
  (if (= b 0) 0
      (+ (remainder b 2) (* 2 (bin->dec (quotient b 10))))))

(define (dec->hex n)
  (define (helper n)
    (if (= n 0) '()
        (if (< (remainder n 16) 10) (cons (char (+ (remainder n 16) 48)) (helper (quotient n 16)))
            (cons (char (+ (remainder n 16) 55)) (helper (quotient n 16))))))
  (if (= n 0) "0"
      (list->string (reverse (helper n)))))

(define (hex->dec s)
  (define (helper l)
    (if (null? l) 0
        (if (>= (int (car l)) 65) (+ (-  (int (car l)) 55) (* 16 (helper (cdr l))))
            (+ (-  (int (car l)) 48) (* 16 (helper (cdr l)))))))
  (helper (reverse (string->list (string-upcase s)))))
 
(define int char->integer)
(define char integer->char)

(define (index-of x l)
  (define (helper x l i)
    (if (eq? x (car l)) i
        (helper x (cdr l) (+ i 1))))
  (helper x l 0))

(define (at-index i l)
  (list-ref l i))

(define (zip f l1 l2)
  (if (or (null? l1) (null? l2))
      '()
      (cons (f (car l1) (car l2)) (zip f (cdr l1) (cdr l2)))))

(define (dec-XOR a b) 
  (let* ([bin-a (dec->bin a)]
         [bin-b (dec->bin b)]
         [bin-XORed (bin-XOR bin-a bin-b)])
    (bin->dec bin-XORed)))

(define (XOR a b)
  (if (and (string? a) (string? b))
      (let ((output (dec->hex (dec-XOR (hex->dec a) (hex->dec b)))))
        (if (= 1 (string-length output))
            (string-append "0" output) output))
      (dec-XOR a b)))

(define (bin-XOR a b)
  (if (and (= a 0) (= b 0)) 0
      (cond [(= (remainder a 10) (remainder b 10))
             (* 10 (bin-XOR (quotient a 10) (quotient b 10)))]
            [else (+ 1 (* 10 (bin-XOR (quotient a 10) (quotient b 10))))])))
 
(define ideal-matrix (matrix (list '(1 2 3 4) '(5 6 7 8) '(9 10 11 12) '(13 14 15 16))))
(define (AddRoundKey state cipher-key)
  (matrix (zip (lambda (l1 l2) (zip XOR l1 l2)) (matrix-rows state) (matrix-rows cipher-key))))

(define (combine-f f n)
    (if (= n 0) (lambda (x) x)
        (if (= n 1) f
            (lambda (x) (f ((combine-f f (- n 1)) x))))))

(define (sublist start length l)
  (if (= start 0)
      (if (= length 0) '()
          (cons (car l) (sublist start (- length 1) (cdr l))))
      (sublist (- start 1) length (cdr l))))

(define (drop-n n l)
  (if (= n 0) l
      (drop-n (- n 1) (cdr l))))

(define (sort-by-freq lst)
  (define (exclude alist key)
    (foldr (lambda (ass result)
             (if (equal? (car ass) key)
                 result
                 (cons ass result)))
           '() alist))
  (sort (foldr (lambda (key bag)
                 (cond ((assoc key bag)
                        => (lambda (old)
                             (let ((new (cons key (+ (cdr old) 1))))
                               (cons new (exclude bag key)))))
                       (else (let ((new (cons key 1)))
                               (cons new bag)))))
               '() lst) (lambda (a b) (> (cdr a) (cdr b)))))

(define (line-wrap s)
  (define (helper i l)
    (if (null? l) '()
        (if (= i 80) (cons #\newline (helper 0 l))
            (cons (car l) (helper (+ i 1) (cdr l))))))
  (let ((l (string->list s)))
    (list->string (helper 0 l))))
