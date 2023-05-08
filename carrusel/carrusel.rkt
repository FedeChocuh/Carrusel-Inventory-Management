#lang racket
(define txtcarrusel (open-input-file "carrusel.txt"))
(define txttransacciones (open-input-file "transacciones.txt"))

(define carrusel (read txtcarrusel))
(define actual (read txtcarrusel))
(define rows (length carrusel))
(define columnas (length(car carrusel)))




; Sacar ubicaciones relativo a su txt
(define coords (xy (apply append carrusel) '() 1 1))


; Acomodar ubicaciones
(define (xy lst f x y)
  (if (null? lst) f
      (if (<= y columnas) (xy (cdr lst) (append f (list (list(caar lst) (cons x y)))) x (+ y 1))
          (xy lst f (+ x 1) 1))))


; Buscar
(define (search prod)
  (cadar (filter (lambda (p) (equal? prod (car p))) coords)))

;
(define (details x y lst)
  (define (element x lst)
    (if (not (= 1 y))
        (element (- x 1) (cdr lst))
        (car lst)))
  (if (not (= 1 x))
      (details (- x 1) y (cdr lst))
      (element y (car lst))))



(define valor-total (apply + (map (lambda (x) (* (cadr x) (caddr x))) (apply append carrusel))))



(define (low-prods lst f)
  (if (null? lst) f
      (low-prods (cdr lst) (append f (list (list (caar lst) (cadar lst) (search (caar lst))))))))

(define low-prod (low-prods (filter (lambda (x) (> 20 (cadr x))) (apply append carrusel)) '()))


(define (a)
  (if (= 0 (- (car (search actual)) 1))
      (begin
        (displayln (details rows (cdr (search actual)) carrusel))
        (write (list carrusel (car (details rows (cdr (search actual)) carrusel))) output))
      (begin
        (displayln (details (- (car (search actual)) 1) (cdr (search actual)) carrusel))
        (write (list carrusel (car (details (- (car (search actual)) 1) (cdr (search actual)) carrusel))) output))))

