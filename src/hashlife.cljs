(ns experiment)

;file:///home/santiago/Documents/Santiago/Webs/learn/cljs/canvas-experiments/index.html

(def g0 [[[false false false false][false false false false][false false false false][false false false false]]
         [[false true true true][false false false false][false false false false][false false false false]]
         [[false false false false][false false false false][false false false true][false false false false]]
         [[false false false false][false false false false][false false false false][false false false false]]])


;; awesome functions for constructing vectors
(defn vi [v i] (reduce nth v i))
(defn vmap [v i]
  (if (coll? i)
    (if (coll? (i 0))
      (map (partial vmap v) i)
      (vi v i))
    (nth v i)))

(defn subsq [g]
  (vmap g [[0 2][1 3][2 0][3 1]]))
(defn midsqh [g1 g2]
  (subsq [(nth g1 1) (nth g2 0) (nth g2 3) (nth g1 2)]))
(defn midsqv [g1 g2]
  (subsq [(nth g1 3) (nth g1 2) (nth g2 1) (nth g2 0)]))
(defn quadto9 [g]
   [(subsq (subsq g)) (midsqh (nth g 0) (nth g 1)) (subsq (nth g 1))
    (midsqv (nth g 1) (nth g 2)) (subsq (nth g 2)) (midsqh (nth g 3) (nth g 2))
    (subsq (nth g 3)) (midsqv (nth g 0) (nth g 3)) (subsq (nth g 0))])
(defn quadto3 [g]
  (vmap g  [[[0 2][0 1][1 0][1 3][2 0][3 1][3 0][0 3][0 0]]
            [[1 3][1 0][1 1][1 2][2 1][2 0][3 1][0 2][0 1]]
            [[2 0][1 3][1 2][2 1][2 2][2 3][3 2][3 1][0 2]]
            [[3 1][0 2][1 3][2 0][2 3][3 2][3 3][3 0][0 3]]]))
(defn s3tosubsq [g]
  (vmap g [[[8][1][0][7]] [[1][2][3][0]] [[0][3][4][5]] [[7][0][5][6]]]))

;; Make an empty 2^n grid
(defn gempty [n]
  (cond
   (= n 0) false
   (= n 1) [false false false false]
   :else (map (constantly (gempty (dec n))) (range 4))))
(defn depth [v]
  (if (coll? v) (inc (depth (nth v 0))) 0))
(defn pad [g]
  (let [e (gempty (dec (depth g)))]
    [[e e (nth g 0) e][e e e (nth g 1)][(nth g 2) e e e][e (nth g 3) e e]]))

;; Paddear segun el futuro del currrent pattern hata una potencia de 2


(def qubit
  (memoize (fn [g]
             (map conway (quadto3 g)))))

(defn result [g]
  (if (coll? (vi g [0 0])) ;(> (depth g) 2)
   (map result (s3tosubsq (quadto9 g)))
   (qubit g)))

(do (def life (iterate (comp result pad) g0)) nil)

(def mresult (memoize result))
(do (def mlife (iterate (comp mresult pad) g0)) nil)


(def latestgen (nth mlife 7))

;; (time (nth life 7))
;; (time (nth mlife 7))



; Drawing Logic from here on...

(defn log [& s] (.log js/console (reduce #(str %1 "\n" %2) s)))
(set-print-fn! log) ;; Necessary for (time ())

(defn width []
  (identity js/innerWidth))
(defn height []
  (identity js/innerHeight))
(defn l []
  (min (width) (height)))

; initialife raphael
(def raphael (js/Raphael. 0 0 (width) (height) ))
(defn clear [color]
  (-> raphael
    (.rect 0 0 (width) (height))
    (.attr "fill" color)))

; prints the game of life quadtree
(defn printg [g ox oy x y]
  (cond
   (= g false) nil
   (= g true) (do
             (-> raphael
                (.rect (+ 2 ox) (+ oy 2) (- x ox 2) (- y oy 2) 15)
                (.attr "fill" "#ccc")
                (.attr "stroke" "#bbb")))
   :else (do
           (-> raphael
                (.rect (+ 4 ox) (+ oy 4) (- x ox 4) (- y oy 4))
                (.attr "stroke" "2px #333"))
           (printg (nth g 0)       ox           oy         (/ (+ x ox) 2)   (/ (+ y oy) 2)
           (printg (nth g 1) (/ (+ x ox) 2)     oy              x           (/ (+ y oy) 2)
           (printg (nth g 2) (/ (+ x ox) 2) (/ (+ y oy) 2)      x                 y
           (printg (nth g 3)       ox       (/ (+ y oy) 2) (/ (+ x ox) 2)         y   )))))))


(clear "#acd")
