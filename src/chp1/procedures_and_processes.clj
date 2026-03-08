(ns chp1.procedures-and-processes)
;; 1.2 Procedures and the Processes They Create
; "
; The ability to visualize the consequences of the actions
; under consideration is crucial to becoming an expert
; programmer, just as it is in any synthetic, creative
; activity.
; "


;; Exercise 1.9
(comment
  ; "
  ; This type of process, characterized by a chain of
  ; deferred operations, is called a recursive process.
  ; Carrying out this process requires that the interpreter
  ; keep track of the operations to be performed later on.
  ; "
  (defn add1
    "This is a recursive process, where each recursive call builds up a chain of deferred increment operations until the base case is reached."
    [a b]
    (if (== a 0)
      b
      (inc (add1 (dec a) b
                 )))
    )
  ; "
  ; In general, an iterative process is one whose state
  ; can be summarized by a fixed number of state variables,
  ; together with a fixed rule that describes how the state
  ; variables should be updated as the process moves from
  ; state to state and an (optional) end test that specifies
  ; conditions under which the process should terminate.
  ; "
  (defn add2
    "This is an iterative process, where each recursive call updates the state variables a and b without building up deferred operations, and the process state can be summarized by a fixed number of state variables."
    [a b]
    (if (== a 0)
      b
      (add2
        (dec a)
        (inc b)
        ))
    )
  )

;; Exercise 1.10
(comment
  (defn A [x y]
    (cond (= y 0) 0
          (= x 0) (* 2 y)
          (= y 1) 2
          :else (A (- x 1) (A x (- y 1)))
      ))
  (A 1 10)
  (A 2 4)
  (A 3 3)
  ; (A 0 n) => $2n$
  ; (A 1 n) => $2A(1, n-1)$
  ;         => $2^(n-1)A(1, 1)$
  ;         => $2^n$
  ; (A 2 n) => $A(1,A(2,(n-1))$
  ;         => $2^2^...2\ (^n 2)$
  )

;; Exercise 1.11
(comment
  ;"
  ; (Tree-recursive processes) ... is more straightforward,
  ; being little more than a translation to lisp of the
  ; definition...
  ;"
  (defn fr
    "Recursive process for computing f"
    [n]
    (if (< n 3)
      n
      (+ (fr (- n 1))
         (* 2 (fr (- n 2)))
         (* 3 (fr (- n 3))))))
  (fr 4)
  ;"
  ; To formulate the iterative algorithm required noticing
  ; that the computation could be recast as an iteration
  ; with ... state variables.
  ;"
  ; Use matrices to denote the state transfer.
  ; f(n)       1 2 3  f(n-1)
  ; f(n-1)  =  1 0 0  f(n-2)
  ; f(n-2)     0 1 0  f(n-3)
  ; Name the three state variables a, b, and c.
  ; a <- a + 2b + 3c
  ; b <- a
  ; c <- b
  (defn fi
    "Iterative process for computing f"
    [n]
    (if (< n 3)
      n
      (loop [a 2  ; f(2)
             b 1  ; f(1)
             c 0  ; f(0)
             cnt 2] ; current n value
        (if (== cnt n)
          a
          (recur (+ a (* 2 b) (* 3 c))
                 a
                 b
                 (inc cnt))))))
    )
)
