#lang racket
(define (read-file file)
  ; Reads the file that contains the carousel and the current state.
  (define input (open-input-file file))
  (define content (read input))
  (close-input-port input)
  content)

(define (get-carousel)
  ; Generates the carousel list every time a transaction is executed.
  (car (read-file "carrusel.txt")))

(define (get-current-state)
  (cadr (read-file "carrusel.txt")))

(define rows (length (get-carousel)))
(define columns (length (car (get-carousel))))

;Get coordinates
(define (get-coordinates lst f x y)
  (if (null? lst) f
      (if (<= y columns)
          (get-coordinates (cdr lst) (append f (list (list (caar lst) (cons x y)))) x (+ y 1))
          (get-coordinates lst f (+ x 1) 1))))
;Crear diccionario
(define (get-dictionary)
  (get-coordinates (apply append (get-carousel)) '() 1 1))

(define (search-product product)
  ; Searches for a product by its name and returns its coordinates.
  (cadar (filter (lambda (p) (equal? product (car p))) (get-dictionary))))

(define (search-product-by-user product)
  ; Searches for a product specified by the user. If it returns false, it means it does not exist.
  (define x (filter (lambda (p) (equal? product (car p))) (get-dictionary)))
  (if (null? x) #f
      (cadar x)))

(define (get-details x y lst)
  ; Receives the coordinates of the product and returns its name, quantity, and price.
  (define (get-element y lst)
    (if (not (= 1 y))
        (get-element (- y 1) (cdr lst))
        (car lst)))
  (if (not (= 1 x))
      (get-details (- x 1) y (cdr lst))
      (get-element y (car lst))))

(define (replace-element product x y lst f)
  ; Changes the quantity of the product in the carousel and then overwrites the file.
  (define (replace-row product y lst f)
    (cond
      ((null? lst) (list f))
      ((= y 1) (replace-row product (- y 1) (cdr lst) (append f (list product))))
      (else (replace-row product (- y 1) (cdr lst) (append f (list (car lst)))))))
  (cond
    ((null? lst) f)
    ((= 1 x) (replace-element product (- x 1) y (cdr lst) (append f (replace-row product y (car lst) '()))))
    (else (replace-element product (- x 1) y (cdr lst) (append f (list (car lst)))))))

(define total-value (apply + (map (lambda (x) (* (cadr x) (caddr x))) (apply append (get-carousel))))) ; Calculates the total value of the inventory.
;Low stock
(define (get-low-stock-products-aux lst f)
  (if (null? lst) f
      (get-low-stock-products-aux (cdr lst) (append f (list (list (caar lst) (cadar lst) (search-product (caar lst))))))))
(define low-stock-products (get-low-stock-products-aux (filter (lambda (x) (> 20 (cadr x))) (apply append (get-carousel))) '())) ; Returns the products with less than 20 units.

(define (write-file lst)
  ; Overwrites the carousel file with the received list.
  (define output (open-output-file "carrusel.txt" #:exists 'replace))
  (write lst output)
  (close-output-port output))
;Arriba
(define (move-up)
  (define coords (search-product (get-current-state)))
  (define lst (get-carousel))
  (if (= 0 (- (car coords) 1))
      (begin
        (displayln (get-details rows (cdr coords) lst)))
      (begin
        (displayln (get-details (- (car coords) 1) (cdr coords) lst))
        (write-file (list (get-carousel) (car (get-details (- (car coords) 1) (cdr coords) lst)))))))
;Abaj
(define (move-down)
  (define coords (search-product (get-current-state)))
  (define lst (get-carousel))
  (if (= rows (+ (car coords) 1))
      (begin
        (displayln (get-details 1 (cdr coords) lst)))
      (begin
        (displayln (get-details (+ (car coords) 1) (cdr coords) lst))
        (write-file (list lst (car (get-details (+ (car coords) 1) (cdr coords) lst)))))))
;Izq
(define (move-left)
  (define coords (search-product (get-current-state)))
  (define lst (get-carousel))
  (if (= 0 (- (cdr coords) 1))
      (displayln "Invalid movement.")
      (begin
        (displayln (get-details (car coords) (- (cdr coords) 1) lst))
        (write-file (list lst (car (get-details (car coords) (- (cdr coords) 1) lst)))))))
;Der
(define (move-right)
  (define coords (search-product (get-current-state)))
  (define lst (get-carousel))
  (if (< columns (+ (cdr coords) 1))
      (displayln "Invalid movement.")
      (begin
        (displayln (get-details (car coords) (+ (cdr coords) 1) lst))
        (write-file (list lst (car (get-details (car coords) (+ (cdr coords) 1) lst)))))))

(define (move-up-aux x1 x2 lst)
  (if (= x1 x2) lst
      (if (= x1 1)
          (move-up-aux rows x2 (append lst (list 'move-up)))
          (move-up-aux (- x1 1) x2 (append lst (list 'move-up))))))

(define (move-down-aux x1 x2 lst)
  (if (= x1 x2) lst
      (if (= x1 rows)
          (move-down-aux 1 x2 (append lst (list 'move-down)))
          (move-down-aux (+ x1 1) x2 (append lst (list 'move-down))))))

(define (get-distance-y y1 y2 lst)
  (if (= y1 y2) lst
      (if (> y1 y2)
          (get-distance-y (- y1 1) y2 (append lst (list 'move-left)))
          (get-distance-y (+ y1 1) y2 (append lst (list 'move-right))))))

(define (get-distance x1 y1 x2 y2 lst)
  ; Calculates the distance between the current product and the product to be reached, determines the fastest route.
  (cond
    ((< x1 x2) (if (> (- x2 x1) (/ rows 2))
                   (get-distance 0 y1 0 y2 (move-up-aux x1 x2 lst))
                   (get-distance 0 y1 0 y2 (move-down-aux x1 x2 lst))))
    ((> x1 x2) (if (> (- x1 x2) (/ rows 2))
                   (get-distance 0 y1 0 y2 (move-down-aux x1 x2 lst))
                   (get-distance 0 y1 0 y2 (move-up-aux x1 x2 lst))))
    (else (begin
            (display "Shortest route: ")
            (displayln (get-distance-y y1 y2 lst))))))

(define (remove-n quantity)
  ; Removes the quantity of the product that is currently displayed.
  (define coords (search-product (get-current-state)))
  (define lst (get-carousel))
  (define product (get-details (car coords) (cdr coords) lst))
  (if (< (- (cadr product) quantity) 0)
      (begin
        (displayln "Attempted to remove more than the existing quantity.")
        (write-file (list (replace-element (list (car product) 0 (caddr product)) (car coords) (cdr coords) lst '()) (car product))))
      (write-file (list (replace-element (list (car product) (- (cadr product) quantity) (caddr product)) (car coords) (cdr coords) lst '()) (car product)))))

(define (remove-p quantity prod)
  ; Removes the quantity of the specified product.
  (define origin (search-product (get-current-state)))
  (define destination (search-product-by-user (car prod)))
  (define lst (get-carousel))
  (if (not destination)
      (displayln "Product not found.")
      (begin
        (if (equal? origin destination)
            (void)
            (get-distance (car origin) (cdr origin) (car destination) (cdr destination) '()))
        (if (< (- (cadr (get-details (car destination) (cdr destination) lst)) quantity) 0)
            (begin
              (displayln "Attempted to remove more than the existing quantity.")
              (write-file (list (replace-element (list (car (get-details (car destination) (cdr destination) lst)) 0 (caddr (get-details (car destination) (cdr destination) lst))) (car destination) (cdr destination) lst '()) (car (get-details (car destination) (cdr destination) lst)))))
            (write-file (list (replace-element (list (car (get-details (car destination) (cdr destination) lst)) (- (cadr (get-details (car destination) (cdr destination) lst)) quantity) (caddr (get-details (car destination) (cdr destination) lst))) (car destination) (cdr destination) lst '()) (car (get-details (car destination) (cdr destination) lst))))))))

(define (remove quantity . prod)
  ; Specifies an optional argument for the remove function.
  (if (null? prod)
      (remove-n quantity)
      (remove-p quantity prod)))

(define (add-n quantity)
  ; Adds the quantity to the product that is currently displayed.
  (define coords (search-product (get-current-state)))
  (define lst (get-carousel))
  (define product (get-details (car coords) (cdr coords) lst))
  (write-file (list (replace-element (list (car product) (+ (cadr product) quantity) (caddr product)) (car coords) (cdr coords) lst '()) (car product))))

(define (add-p quantity prod)
  ; Adds the quantity to the specified product.
  (define origin (search-product (get-current-state)))
  (define destination (search-product-by-user (car prod)))
  (define lst (get-carousel))
  (if (not destination)
      (displayln "Product not found.")
      (begin
        (if (equal? origin destination)
            (void)
            (get-distance (car origin) (cdr origin) (car destination) (cdr destination) '()))
        (write-file (list (replace-element (list (car (get-details (car destination) (cdr destination) lst)) (+ (cadr (get-details (car destination) (cdr destination) lst)) quantity) (caddr (get-details (car destination) (cdr destination) lst))) (car destination) (cdr destination) lst '()) (car (get-details (car destination) (cdr destination) lst)))))))

(define (add quantity . prod)
  ; Specifies an optional argument for the add function.
  (if (null? prod)
      (add-n quantity)
      (add-p quantity prod)))

(define (execute-transactions file)
  ; Reads and executes transactions line by line.
  (load file))

;(execute-transactions "transacciones.txt")
(display (get-carousel))
(newline)
(search-product "Azúcar")
(display "Total inventory value: ") (displayln total-value)
(newline)
(display "Products with low or no inventory: ") (displayln low-stock-products)
