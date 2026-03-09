(ns chp1.elements 
  (:require
   [clojure.math :as math]))
;; Chapter 1: Building Abstractions with Procedures
;; 1.1 The Elements of Programming


;; Exercise 1.1
(comment
  ; Note the syntax differences between Clojure and Scheme
  (def a 3)
  (def b (+ a 1))
  (if (and (> b a) (< b (* a b)))
    b
    a)
  (cond (= a 4) 6
        (= b 4) (+ 6 7 a)
        :else 25)
  (+ 2 (if (> b a) b a))
  (* (cond (> a b) a
           (< a b) b
           :else -1)
     (+ a 1))
  )

;; Exercise 1.2
(comment
  (/ (+ 5
        4
        (- 2
           (- 3
              (+ 6
                 (/ 4
                    5)))))
     (* 3
        (- 6 2)
        (- 2 7)))
  )

;; Exercise 1.3
(comment
  ; Syntax differences: def & defn vs define
  (defn square [a] (* a a))
  (defn e1_3 [a b c]
    (cond (and (< a b) (< a c)) (+ (square b) (square c))
          (and (< b a) (< b c)) (+ (square a) (square c))
          :else (+ (square a) (square b))))
  (e1_3 3 4 2) 
  )

;; Exercise 1.4
(comment
  ; The interpreter first evaluates the leftmost element,
  ; which determines whether the operator is `+` or `-`.
  ; This effectively computes the sum of `a` and the absolute value of `b`.
  (defn a-plus-abs-b [a b]
    ((if (> b 0) + -) a b))
  (a-plus-abs-b 1 -2)
  )

;; Exercise 1.5
(comment
  (defn p []
    (p))
  (defn test [x y]
    (if (= x 0) x y))
  (test 2 3)
  (test 0 3)
  ; Clojure, like Scheme, uses applicative-order evaluation.
  ; The interpreter evaluates all arguments before applying the function,
  ; including the procedure `p`, which results in infinite recursion.
  ; Therefore, the following expression will cause a StackOverflowError.
  (test 0 (p))
  ; With normal-order evaluation, the interpreter would not evaluate `p` immediately.
  ; Instead, it would fully expand the expression to:
  ; ((if (= x 0) x y) 0 (p))
  ; The `if` special form would then evaluate the condition (= 0 0),
  ; return 0, and avoid evaluating the recursive call to `p`.
  )


;; Exercise 1.6 to 1.8
(comment
  ; Original square root implementation using Newton's method
  (declare good-enough?)
  (declare improve)
  (defn sqrt-iter [guess x]
    (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
  (defn average [x y]
    (/ (+ x y) 2))
  (defn improve [guess x]
    (average guess (/ x guess)))
  (defn square [x] (* x x))
  (defn good-enough? [guess x]
    (< (abs (- (square guess) x)) 0.001))
  (defn sqrt-original [x]
    (sqrt-iter 1.0 x))
  (sqrt-original 65536)
  ; Exercise 1.6
  ; Here is the proposed `new-if`
  (defn new-if [predicate then-clause else-clause]
    (cond
      predicate then-clause
      :else else-clause))
  ; Square root implementation using `new-if` instead of the special form `if`
  (defn sqrt-iter-new [guess x]
    (new-if (good-enough? guess x)
            guess
            (sqrt-iter-new (improve guess x) x)))
  (defn sqrt-new [x] (sqrt-iter-new 1.0 x))
  ; Attempt to compute with sqrt-new:
  ; (sqrt-new 65536)
  ; The interpreter returns a StackOverflowError.
  ; Although syntactically correct, evaluating the `new-if` function
  ; requires evaluating all its arguments before application (see Exercise 1.5).
  ; This means both the `then-clause` and `else-clause` are evaluated
  ; regardless of the predicate's value, leading to a larger recursion depth
  ; The special form `if`, in contrast, only evaluates the relevant branch
  ; based on the predicate's truth value, making it suitable for control flow.
  ;
  ; Exercise 1.7
  ; First try to compute the square root of a small number
  (sqrt-original 1e-4)
  ; The result is inaccurate because `good-enough?` compares the error
  ; against a fixed threshold of 0.001, which is relatively large
  ; for small numbers like 0.0001.
  ; Now check how the function works for a larger input. 
  (def a-large-number 0xFFFFFFFFFFFFFFFF)
  (math/sqrt a-large-number)
  ; 4.294967296E9
  (sqrt-original a-large-number)
  ; 4.294967296E9
  ; The function behaves differently from the Book's hint  
  ; and produces a reasonable result. Modern Clojure implementations
  ; on contemporary hardware can handle very large numbers efficiently
  ; due to improved numeric type representations and precision.
  ; However, when the number goes large enough or the hardware
  ; is old enough, the function would fail as in the Book.
  ; This is because smaller differences like 0.001
  ; in large number could be truncated when the numeric range 
  ; is limited, and the `good-enough` could not tell 
  ; the square of a bad guess from the input.
  ; Here is the alternative implementation of `good-enough`
  ; where the difference is 'normalized' into a ratio
  ; regardless of the actual numeric size of guess.
  (defn improved-enough? [guess prev]
    (< (/ (abs (- guess prev)) prev) 1e-6))
  ; Square root implementation using `improved-enough` 
  ; instead of `good-enough`
  (defn sqrt-iter-alt [guess x prev-guess]
    (if (improved-enough? guess prev-guess)
      guess
      (sqrt-iter-alt (improve guess x) x guess)))
  (defn sqrt-alt [x] (sqrt-iter-alt 1.0 x 0.001))
  ; Test it with small, regular and large cases
  (sqrt-alt 1e-4)
  (sqrt-alt 36)
  (sqrt-alt a-large-number)
  ;
  ; Exercise 1.8
  ; The sole difference between the cube-root procedure 
  ; and the square-root procedure is the way to 
  ; improve the guess. Newton's Method is an 
  ; abstraction for both of them.
  (defn cbrt-improve [guess x]
    (/ (+ (/ x (square guess)) (* 2 guess)) 3.0))
  (defn cbrt-iter [guess x prev-guess]
    (if (improved-enough? guess prev-guess)
      guess
      (cbrt-iter (cbrt-improve guess x) x guess)))
  (defn cbrt [x] (cbrt-iter 1.0 x 0.001))
  ; Tests of the cube-root procedure
  (cbrt 1e-6)
  (cbrt 729)
  (cbrt a-large-number)
  (math/cbrt a-large-number)
  )