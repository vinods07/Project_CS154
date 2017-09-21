#lang racket

(require "declarations.rkt")

(provide caesar-shift-Hack)
(provide scytale-Hack)

(define (nice-text? text)
  
  (define total-points 0)
  
  (define (points c)
    (cond ((eq? c #\E) 24)
          ((eq? c #\T) 18)
          ((eq? c #\A) 16)
          ((eq? c #\O) 15)
          ((or (eq? c #\I) (eq? c #\N)) 14)
          ((eq? c #\S) 13)
          ((or (eq? c #\H) (eq? c #\R)) 12)
          ((eq? c #\D) 9)
          ((eq? c #\I) 8)
          ((or (eq? c #\C) (eq? c #\U)) 6)
          ((or (eq? c #\M) (eq? c #\W) (eq? c #\F)) 5)
          ((or (eq? c #\G) (eq? c #\Y) (eq? c #\P)) 4)
          ((eq? c #\B) 3)
          ((or (eq? c #\V) (eq? c #\K)) 2)
          ((or (eq? c #\J) (eq? c #\X) (eq? c #\Q) (eq? c #\Z)) 1)
          (else 0)))
  (define (points2 c)
    (cond ((and (eq? (car c) #\T) (eq? (cdr c) #\H)) 15)
          ((and (eq? (car c) #\H) (eq? (cdr c) #\E)) 13)
          ((and (eq? (car c) #\I) (eq? (cdr c) #\N)) 9)
          ((and (eq? (car c) #\E) (eq? (cdr c) #\R)) 9)
          ((and (eq? (car c) #\A) (eq? (cdr c) #\N)) 8)
          ((and (eq? (car c) #\R) (eq? (cdr c) #\E)) 7)
          ((or (and (eq? (car c) #\N) (eq? (cdr c) #\D))
               (and (eq? (car c) #\A) (eq? (cdr c) #\T))
               (and (eq? (car c) #\O) (eq? (cdr c) #\N))
               (and (eq? (car c) #\N) (eq? (cdr c) #\T))
               (and (eq? (car c) #\H) (eq? (cdr c) #\A))
               (and (eq? (car c) #\E) (eq? (cdr c) #\S))
               (and (eq? (car c) #\S) (eq? (cdr c) #\T))
               (and (eq? (car c) #\E) (eq? (cdr c) #\N))) 6)
          ((or (and (eq? (car c) #\E) (eq? (cdr c) #\D))
               (and (eq? (car c) #\T) (eq? (cdr c) #\O))
               (and (eq? (car c) #\I) (eq? (cdr c) #\T))
               (and (eq? (car c) #\O) (eq? (cdr c) #\U))
               (and (eq? (car c) #\E) (eq? (cdr c) #\A))
               (and (eq? (car c) #\H) (eq? (cdr c) #\I))
               (and (eq? (car c) #\I) (eq? (cdr c) #\S))) 5)
          ((and (eq? (car c) #\O) (eq? (cdr c) #\R)) 4)
          ((or (and (eq? (car c) #\T) (eq? (cdr c) #\I))
               (and (eq? (car c) #\A) (eq? (cdr c) #\S))
               (and (eq? (car c) #\T) (eq? (cdr c) #\E))) 3)
          ((or (and (eq? (car c) #\E) (eq? (cdr c) #\T))
               (and (eq? (car c) #\N) (eq? (cdr c) #\G))
               (and (eq? (car c) #\O) (eq? (cdr c) #\F))) 2)
          ((or (and (eq? (car c) #\A) (eq? (cdr c) #\L))
               (and (eq? (car c) #\D) (eq? (cdr c) #\E))
               (and (eq? (car c) #\L) (eq? (cdr c) #\E))
               (and (eq? (car c) #\S) (eq? (cdr c) #\E))
               (and (eq? (car c) #\S) (eq? (cdr c) #\A))
               (and (eq? (car c) #\S) (eq? (cdr c) #\I))) 1)
          (else 0)))

    
  (let* ([char-list (string->list (string-upcase text))]
         [only-alphabet-length (foldr (lambda (x y) (if (alphabet? (int x)) (+ 1 y) y)) 0 char-list)]
         [1letter (sort-by-freq char-list)]
         [1letter-points (/ (foldr (lambda (x y) (+ (* 1.0 (cdr x) (points (car x))) y)) 0 1letter) only-alphabet-length)]
         [2letter (sort-by-freq (foldr (lambda (x y) (if (null? x) y (cons x y))) '()
                                       (zip (lambda (x y) (if (and (alphabet? x) (alphabet? y)) (cons x y) '())) char-list (cdr char-list))))]
         [2letter-points (/ (foldr (lambda (x y) (+ (* 1.0 (cdr x) (points2 (car x))) y)) 0 2letter) only-alphabet-length)])
         
    (cons text (+ 1letter-points 2letter-points))))

(define (caesar-shift-Hack msg)

  (define (change c k) ;;takes ascii int and applies key to output ascii int decrypt
    (let* ([i (+ c k)])
      (cond [(= k 0) c]
            [(alphabet? c)  (+ (modulo (- i 65) 26) 65)]
            [else  c])))
  (define (key-guess msg key) ; gives ascii int list of decrypted msg for given key
    (let* ([int-list (map int (string->list msg))]
           [guess-decrypt (map (lambda (x)  (change x (* key -1))) int-list)])
      (list->string (map char guess-decrypt))))

  (define (all-guesses msg key)
    (if (> key 25)
        '()
        (cons (key-guess msg key) (all-guesses msg (+ key 1)))))

  (let* ([all-possible-keys (all-guesses msg 1)]
         [nice-check (map nice-text? all-possible-keys)]
         [sorted (sort nice-check (lambda (x y) (> (cdr x) (cdr y))))]
         [most-appropriet-msg (car sorted)])
     (car most-appropriet-msg)))

(define (scytale-Hack msg)

  (define (scytale-key-guess text k)
  
    (define (group-list l n)
      (if (null? l) '()
          (append (list (sublist 0 n l)) (group-list (drop-n n l) n))))
    
    (define (degroup L)
      (if (or (null? L) (null? (car L))) '()
          (append (map car L) (degroup (map cdr L)))))
    
    (let* ([char-list (string->list text)]
           [fit-list ( (combine-f (lambda (x) (append x (list (char 32))))
                                  (modulo (- k (modulo (string-length text) k)) k)) char-list)]
           [L (group-list fit-list k)]
           [otuput (list->string (degroup L))])
      
      otuput))
  
  (define (try-all-keys msg i)
    (if (= i 0) '()
        (cons (scytale-key-guess msg (quotient (string-length msg) i)) (try-all-keys msg (- i 1)))))
  
  (let* ([L (try-all-keys msg (string-length msg))](print L)
                                                   [checked-list (map nice-text? L)]
                                                   [sorted-check-list (sort checked-list (lambda (a b) (> (cdr a) (cdr b))))]
                                                   [best-output (car (car sorted-check-list))])
    best-output))
