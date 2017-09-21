#lang racket

(require "declarations.rkt")

(provide scytale-encryption scytale-decryption)

(define (trim-spaces text)
    (define (helper l)
      (if (null? l) '()
          (if (eq? (car l) (char 32))
              (helper (cdr l))
              (reverse l))))
    (let* ([l (string->list text)]
           [trimmed (helper (reverse l))])
      (list->string trimmed)))

(define (scytale-general text k)
  
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
         [cipher-otuput (list->string (degroup L))])
    
    cipher-otuput))

(define key 1)
(define (generate-random-key len)
  (set! key (random 2 (quotient len 2))))
(define (key-enter k)
  (set! key k))

(define (scytale-encryption msg)
  (begin (generate-random-key (string-length msg))
         (scytale-general msg key)))

(define (scytale-decryption cipher-text)
  (trim-spaces (scytale-general cipher-text (quotient (string-length cipher-text) key))))

