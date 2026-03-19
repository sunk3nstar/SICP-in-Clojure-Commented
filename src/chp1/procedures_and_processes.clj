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

;; Exercise 1.14
(comment
  ; `tap` in clojure is convenient for debug print.
  (add-tap println)

  (declare cc)
  (declare first-denomination)
  (defn count-change
    [amount]
    (cc amount 5)
    )
  (defn cc [amount kind-of-coins]
    (tap> [::cc amount kind-of-coins])
    (cond (= amount 0) 1
          (or (< amount 0) (= kind-of-coins 0)) 0
          :else (+ (cc amount (- kind-of-coins 1))
                   (cc (- amount (first-denomination kind-of-coins)) kind-of-coins))
          )
    )
  (defn first-denomination [kind-of-coins]
    (cond (= kind-of-coins 1) 1
          (= kind-of-coins 2) 5
          (= kind-of-coins 3) 10
          (= kind-of-coins 4) 25
          (= kind-of-coins 5) 50)
    )
  (count-change 11)

  ;"
  ; In general, the number of steps required by a tree-recursive process will be
  ; proportional to the number of nodes in the tree, while the space required
  ; will be proportional to the maximum depth of the tree.
  ;"
  ; The space complexity is O(n) obviously.
  ; For the time complexity, note that the in general (cc n k) = (cc n k-1) + (cc n-d_k k),
  ; when n goes large enough (cc n k) \approx (cc n k-1) + (cc n-1 k-1) \approx n^k
  ; in this case the time complexity is O(n^5).
  )

;; Exercise 1.15
(comment
  (defn cube [x] (* x x x))
  (defn p [x] (- (* 3 x) (* 4 (cube x))))
  (defn sine [angle]
    (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

  ; The process divide the angle by 3 repeatedly until it goes below 0.1.
  ; So for angle `a`, the number of steps `n` satisfies
  ; $\frac{|a|}{3^n}<0.1$. Consequently, the time complexity is O(log n)
  ; This is a linear recursive process, where each call of `sine` has to
  ; be saved. Therefore, the space needed grows at the same pace with
  ; the number of steps, which is O(log n).
  )

;; Exercise 1.16
(comment
  ; b^n=b\times b^(n-1) when n is odd.
  ; b^n=b^(n/2)*b^(n/2)=(b^2)^(n/2) when n is even, which makes it
  ; possible to exponentially reduce the complexity.
  (defn fast-expo [base pow]
    (defn is-even? [n] (= (mod n 2) 0))
    (defn fast-expo-iter [base pow accumulation]
      ; Instead of (* accumulation (fast-expo ...)), we carry the
      ; accumulation into the next call as a state. So, it will not
      ; be necessary to record the current call for later use.
      (cond (= pow 0) accumulation
            (is-even? pow) (fast-expo-iter (* base base) (/ pow 2) accumulation)
            :else (fast-expo-iter base (- pow 1) (* accumulation base))
            )
      )
    (fast-expo-iter base pow 1)
    )
  (fast-expo 5 3)
  (fast-expo 2 16)
  )

;; Exercise 1.17 & 1.18
(comment
  (defn is-even? [n] (= (mod n 2) 0))
  (defn double-mul [n] (+ n n))
  (defn halve [n] (quot n 2))
  ; Exercise 1.17
  (defn rpm-recursive
    "'This algorithm, which is sometimes known as the *Russian peasant method* of multiplication, is ancient'"
    [a b]
    (cond (= b 0) 0
          (is-even? b) (double-mul (rpm-recursive a (halve b)))
          :else (+ a (rpm-recursive a (- b 1)))
          )
    )
  (rpm-recursive 25 36)
  ; Exercise 1.18
  (defn rpm-iterative [a b]
    (defn rpm-iter [x y accumulation]
      (cond (= y 0) accumulation
            (is-even? y) (rpm-iter (double-mul x) (halve y) accumulation)
            :else (rpm-iter x (- y 1) (+ accumulation x)))
      )
    (rpm-iter a b 0)
    )
  (rpm-iterative 25 36)
  )

;; Exercise 1.19
(comment
  ; Tpq: a <- bq + aq + ap  b <- bp + aq
  ; Calculate brutally, we have
  ; Tpq^2: a <- b(q^2 + 2pq) + a(q^2 + 2pq) + a(p^2 + q^2)
  ;        b <- b(p^2 + q^2) + a(q^2 + 2pq)
  ; Tpq^2 = T(p^2 + q^2)(q^2 + 2pq)
  (defn is-even? [n] (= (mod n 2) 0))
  (declare fib-iter)
  (defn fib
    "To compute fib(n) is to compute T01^n(1,0)"
    [n] (fib-iter 1 0 0 1 n))
  (defn fib-iter [a b p q count]
    (cond (= count 0) b
          ; when n is even:
          ; Tpq^n(a,b) = Tpq(Tpq...Tpq(a,b)) (n times)
          ;            = Tpq^2(Tpq^2...Tpq^2(a,b)) (n/2 times)
          ;            = T(p^2 + q^2)(q^2 + 2pq)^(n/2)(a,b)
          (is-even? count) (fib-iter a, b, (+ (* p p) (* q q)), (+ (* q q) (* 2 p q)) (/ count 2))
          ; when n is odd:
          ; Tpq^n(a,b) = Tpq^(n-1)(Tpq(a,b))
          :else (fib-iter (+ (* b q) (* a q) (* a p)), (+ (* b p) (* a q)), p, q, (- count 1))
          ))
  (fib 7)
  (fib 50)
  )