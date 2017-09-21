#lang racket

(require "declarations.rkt")

(provide caesar-shift-encryptor caesar-shift-decryptor)

(define key 0)
(define (generate-random-key) (set! key (+ 1 (random 24))))

(define (change c f) 
  (let* ([i (f (int c) key)])
    (cond [(= key 0) c]
          [(alphabet? c) (char (+ (modulo (- i 65) 26) 65))]
          [else c])))

(define (general msg f)
  (let* ([char-list (string->list msg)]
         [key-applied-list (map (lambda (c) (change c f)) char-list)]
         [changed-msg (list->string key-applied-list)])
    changed-msg))

(define (caesar-shift-encryptor msg cskey)
  (if (eq? "random" cskey) (begin (generate-random-key)
                                  (general msg +))
      (begin (set! key cskey)
             (general msg +))))

(define (caesar-shift-decryptor msg)
  (general msg -))

  

