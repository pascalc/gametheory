(ns gametheory.core
  (:refer-clojure :rename {== equal-nums?})
  (:use [clojure.core.logic]))

(defne take-row 
  [matrix row rows-left]
  ([m r rs]
    (rembero row m rs)))

(defne row
  [matrix row]
  ([m r]
    (fresh [rs]
      (take-row m r rs))))

(defne first-column
  [m fc rest-matrix]
  ([ [] [] [] ])
  ([ [[] . _] [] [] ])
  ([ [[i . is] . rs] [i . col] [is . res] ]
    (first-column rs col res)))

; transpose([], []).
; transpose([F|Fs], Ts) :-
;     transpose(F, [F|Fs], Ts).
; transpose([], _, []).
; transpose([_|Rs], Ms, [Ts|Tss]) :-
;         lists_firsts_rests(Ms, Ts, Ms1),
;         transpose(Rs, Ms1, Tss).

(defne transpose-helper
  [first-col rows-left columns]
  ([ []  _ [] ])
  ([ [_ . rs] ms [ts . tss] ]
    (fresh [ms1]
      (first-column ms ts ms1)
      (transpose-helper rs ms1 tss))))

(defne transpose
  [m mT]
  ([ [] [] ])
  ([ [f . fs] ts ]
    (fresh [matrix]
      (conso f fs matrix)
      (transpose-helper f matrix ts))))

(defne take-column
  [matrix col cols-left]
  ([m c cs]
    (fresh [mT]
      (transpose m mT)
      (take-row mT c cs))))

(defne column
  [matrix col]
  ([m c]
    (fresh [cs]
      (take-column m c cs))))

(defne weakly-dominates
  [l1 l2]
  ([ [] [] ])
  ([ [h1 . t1] [h2 . t2] ]
    (project [h1 h2]
      (== true (>= h1 h2)))
    (weakly-dominates t1 t2)))

(defne strongly-dominates
  [l1 l2]
  ([ x y ]
    (!= x y) ;; Doesn't work for columns?!
    (weakly-dominates x y)))

(defne row-pair
  [matrix row1 row2]
  ([m r1 r2]
    (fresh [left1 left2]
      (take-row m r1 left1)
      (take-row left1 r2 left2))))

(defne col-pair 
  [matrix col1 col2]
  ([m c1 c2]
    (fresh [mT]
      (transpose m mT)
      (row-pair mT c1 c2))))

(defne dominates-row
  [m r1 r2]
  ([ matrix row1 row2 ]
    (row-pair matrix row1 row2)
    (strongly-dominates row1 row2)))

(defne dominates-column
  [m c1 c2]
  ([ matrix col1 col2 ]
    (col-pair matrix col1 col2)
    ;(strongly-dominates col1 col2))) <-- Fix this
    (weakly-dominates col1 col2)))

(defne nullified
  [l nils]
  ([ [] [] ])
  ([ [_ . t] [nil . nls] ]
    (nullified t nls)))

(defne nullify-row
  [matrix row nullified-m]
  ([ [] _ [] ])
  ([ [m . ms] r [nl . nls] ]
    (!= m r)
    (nullify-row ms r nls))
  ([ [r . ms] r [nl . nls] ]
    (nullified r nl)
    (nullify-row ms r nls))) ;; => Unbound lvars everywhere

(defne nullify-column
  [matrix column nullified-m]
  ([m c nlm]
    (fresh [mT nlmT]
      (transpose m mT)
      (nullify-row mT c nlmT)
      (transpose nlmT nlm)))) ;; => Infinite regress

;; REPL

(defrel matrix m)
(fact matrix '((1 2 3) (4 5 6) (7 8 9)))
