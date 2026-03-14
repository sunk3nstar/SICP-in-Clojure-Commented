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
  ; $(A\ 0\ n) \Rightarrow 2n$
  ; $(A\ 1\ n) \Rightarrow 2 \cdot A(1, n-1)$
  ;         $\Rightarrow 2^{n-1} \cdot A(1, 1)$
  ;         $\Rightarrow 2^n$
  ; $(A\ 2\ n) \Rightarrow A(1, A(2, (n-1)))$
  ;         $\Rightarrow 2^{2^{\cdot^{\cdot^{\cdot^{2}}}}}$ (a tower of exponents of height $n$)
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
      (loop [a 2                                            ; f(2)
             b 1                                            ; f(1)
             c 0                                            ; f(0)
             cnt 2]                                         ; current n value
        (if (== cnt n)
          a
          (recur (+ a (* 2 b) (* 3 c))
                 a
                 b
                 (inc cnt))))))
  (fi 4)
  (== (fr 20) (fi 20))
  )

;; Exercise 1.12
(comment
  ; Denote Pascal's triangle as
  ; C_0^0
  ; C_1^0 C_1^1
  ; C_2^0 C_2^1 C_2^2
  ; ...
  ; There is $C_m^n=C_{m-1}^n+C_{m-1}^{n-1}$
  (defn C
    [m n]
    (cond (= n 0) 1
          (= m n) 1
          :else (+ (C (- m 1) n) (C (- m 1) (- n 1))))
    )
  (C 4 2)
  )

;; Exercise 1.13
; Denote the state transfer with matrices:
; 
; $\begin{pmatrix} F_{n} \\ F_{n-1} \end{pmatrix}
;  = \begin{pmatrix} 1 & 1 \\ 1 & 0 \end{pmatrix}
;    \begin{pmatrix} F_{n-1} \\ F_{n-2} \end{pmatrix}$
;
; So $\begin{pmatrix} F_{n+1} \\ F_{n} \end{pmatrix} = A^n \begin{pmatrix} F_{1} \\ F_{0} \end{pmatrix}$
; where $A = \begin{pmatrix} 1 & 1 \\ 1 & 0 \end{pmatrix}$.
;
; To compute $A^n$, use the Cayley–Hamilton theorem:
; $A^2 - A - I = 0$, where $I$ is the identity matrix.
; The eigenvalues of $A$ are $\varphi = \frac{1+\sqrt{5}}{2}$ and $\phi = \frac{1-\sqrt{5}}{2}$.
;
; Choose $a$ and $b$ such that $x^n = q(x)(x^2 - x - 1) + a x + b$.
; Then $\varphi^n = a \varphi + b$ and $\phi^n = a \phi + b$.
;
; Consequently, $A^n = a A + b I = \begin{pmatrix} a+b & a \\ a & b \end{pmatrix}$.
;
; Since $F_n$ is the (1,2) entry of $A^n$ (or noting that $F_n = a F_1 = a$),
; we obtain $F_n = a$.
;
; Solving for $a$ completes the proof.
