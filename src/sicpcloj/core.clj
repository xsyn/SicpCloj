(ns sicpcloj.core)

;; Chapter 1

;; Helper functions

;; Square
(defn square [x]
  (* x x))

;; Absolute

(defn abs [x]
  (cond
   (< x 0) (- x )
   :else x))

;; Square root

(defn average [x y]
  (/ (+ x y) 2))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn good-enough? [guess x]
  (< (abs (- (square guess) x)) 0.001))

(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)))

(defn sqrt [x]
  (sqrt-iter 1.0 x))

;; Excercise 1.3


(defn onethree [x y z]
  (cond
   (and (> x y) (> y z)) (+ (square x) (square y))
   (and (> z y) (> y x)) (+ (square z) (square y))
   :else (+ (square x) (square z))))

;; Excercise 1.7
;;
;; Design a sqrt tester that watches how 'guess' changes from one
;;iteration to the next and stop when it is a small fraction of the guess.
;;
;;
;; As e17sqrt-iter goes through, it divides by the difference, make
;;guesses on smaller iterations of the initial guess.

(defn e17sqrt [n]
  (defn e17good-enough? [old-guess new-guess]
    (< (/ (Math/abs (- old-guess new-guess))
          old-guess)
       0.001))
  
  (defn e17improve [guess]
    (defn e17average [x y] (/ (+ x y) 2))
    (e17average guess (/ n guess)))
  
  (defn help [guess]
    (let [new (e17improve guess)]
      (if (e17good-enough? guess new) new
          (help new))))
    (help 1.0))


;; Excercise 1.8
;;
;; Repurpose the sqrt mehod above to cube root, using the formula:
;;
;; (/ (+ (/ x (square y)) (* 2 y)) 3)

(defn cube [x]
  (defn cubegood-enough? [old-guess guess]
    (< (/ (Math/abs (- old-guess guess))
          old-guess)
       0.001))

  (defn cubeimprove [guess x]
    (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

  (defn approx [seed x]
    (let [new (cubeimprove seed x)]
      (if (cubegood-enough? seed new)
        new
        (approx new x))))
  
  (approx 1.0 x))

;; Excercise 1.10
;;
;; Ackermann's function

(defn A [x y]
  (cond
   (= y 0) 0
   (= x 0) (* 2 y)
   (= y 1) 2
   :else (A (- x 1)
         (A x (- y 1)))))

;; (A 1 10)
;; -> 1024
;; (A 2 4)
;; -> 65536
;; (A 3 3)
;; -> 65536

;; Excercise 1.11
;;
;; A function f is defined by the rule that f(n) = n if n<3 and f(n) =
;;f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3. Write a procedure that
;;computes f by means of a recursive process. Write a procedure that
;;computes f by means of an iterative process.
;;
;; Iterate through use of a counter, always pass back all variables to
;;keep track of state.


(defn frec [n]
  (if
   (< n 3) n
          (+
            (frec (- n 1))
            (* 2 (frec (- n 2)))
            (* 3 (frec (- n 3))))))

(defn fiter-iter [a b c n]
  (cond (= n 0) c
        (= n 1) b
        (= n 2) a
        :else (fiter-iter
               (+ a (* 2 b) (* 3 c))
               a
               b
               (dec n))))

(defn fiter [n]
  (fiter-iter 2 1 0 n))

;; “Exercise 1.12.

;; The following pattern of numbers is called
;; Pascal's triangle.
;; The numbers at the edge of the triangle are all 1, and each number
;; inside the triangle is the sum of the two numbers aboveit.
;; Write a procedure that computes elements of Pascal's triangle by means of a recursive process.”

(defn pascal [row]
  "Prints Pascal's triangle."
  (do
    (println row)
    (if (= 1 (count row))
      (pascal (reduce conj row row))
      (pascal (into [] (concat [(first row)] (vec (map ( partial apply +) (partition 2 1 row))) [(last row)] ))))))

(defn -main [] )
