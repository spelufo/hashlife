
;file:///home/santiago/docs/webs/learn/cljs/hashlife/index.html

(ns hashlife)

(def g0 [[[false false false false][false false false false][false false false false][false false false false]]
         [[false false true  false][false false false true ][true  false false false][false true  false false]]
         [[false false false false][false false false false][false false false false][false false false false]]
         [[false true  true  false][false false true  false][true  false false false][false true  false false]]])

(def g0 (mapv (constantly g0) (range 4)))
(def g0 (mapv (constantly g0) (range 4)))

(defn rule [spawn survive]
  (fn [nine]
    (contains? (if (nine 0) survive spawn) (count (filter identity (rest nine))))))

(def conway (rule #{3} #{2 3}))

(defn mv [g1 g2]
  [(g1 3) (g1 2) (g2 1) (g2 0)])

(defn mh [g1 g2]
  [(g1 1) (g2 0) (g2 3) (g1 2)])


(defn subquad [g]
  (let [[[ __ __ nw __ ]
         [ __ __ __ ne ]
         [ se __ __ __ ]
         [ __ sw __ __ ]] g]
    [nw ne se sw]))

(defn neigover [g]
   [(subquad g)
    (mh (g 0) (g 1))
    (g 1)
    (mv (g 1) (g 2))
    (g 2)
    (mh (g 3) (g 2))
    (g 3)
    (mv (g 0) (g 3))
    (g 0)])

(defn quadover [g]
  [[(g 8) (g 1) (g 0) (g 7)]
   [(g 1) (g 2) (g 3) (g 0)]
   [(g 0) (g 3) (g 4) (g 5)]
   [(g 7) (g 0) (g 5) (g 6)]])


(defn quad2->neigover [g]
  (let [[[a b c d]
         [e f g h]
         [k l m n]
         [o p q r]] g]
    [[c b e h k p o d a]
     [h e f g l k p c b]
     [k h g l m n q p c]
     [p c h k n q r o d]]))

(def qubit
  (memoize (fn [g]
             (mapv conway (quad2->neigover g)))))

(defn result [g]
  (if (vector? ((g 0) 0))
    (mapv result (quadover (mapv result (neigover g))))
    (qubit g)))

(defn halfresult [g]
  (if (vector? ((g 0) 0))
    (mapv subquad (quadover (mapv result (neigover g))))
    (qubit g)))

(draw g0)

(draw (mapv (comp neigsubquad neighalf) (neigover g0)))
(draw (mapv (comp neigsubquad neighalf) (neigover g0)))
(draw (neighalf g0))
(draw (result g0))

(defn neigsubquad [g] (mapv subquad (quadover g)))
(defn neighalf [g] (mapv halfresult (neigover g)))

(defn result-at-time [g t]
  (let [n (depth g)
        time-to-result (pow 2 (- n 2))
        time-to-halfresult (pow 2 (- n 3))]
    (cond (= 0 t) (subquad g)
          (< t time-to-result) (quadover (mapv #(time-to-result % (/ time-to-result 2)) (neigover g)))
          :else (result-at-time (result g) (- t time-to-result))


(def mresult (memoize result))

(defn depth [v]
  (if (vector? v) (inc (depth (v 0))) 0))

(defn gempty [n]
  (if (= n 0) false
   (mapv (constantly (gempty (dec n))) (range 4))))

(defn pad-by [g r]
  (if (= 0 r) g
   (let [e (gempty (depth (g 0)))]
     (pad-by [[e e (g 0) e][e e e (g 1)][(g 2) e e e][e (g 3) e e]] (dec r)))))

(defn lg [x b]
  (/ (.log js/Math x) (.log js/Math b)))

(defn pow [b x]
  (.pow js/Math b x))

(defn ceil [x]
  (.ceil js/Math x))

(defn result-at-time [g t]
  (let [n (depth g)
        time-to-result (pow 2 (- n 2))
        time-to-halfresult (pow 2 (- n 3))]
    (cond (= 0 t) g
          (<= t time-to-halfresult) (mapv halfresult (neigover g))
          (< t time-to-result) (neign->quadn (mapv #(result-at-time % (- t time-to-intermediate)) (mapv result (quadn->neign g)))
          :else (result-at-time (result g) (- t time-to-result))))))

;; (defn future [g t]
;;   (let [n (ceil (log t 2))
;;         g (pad-by g n)]
;;     (cond
;;      (= 0 n) g
;;      (< (pow 2 (- n 2)) t) (mapv #(future % (- t (pow 2 (- n 2)))) (quadn->neign g))
;;      (< .....))))


;;      (= 1 g) ))
;;   (= 0 (mod t 2)
;;    (< t 0) nil
;;    (= t 0) g
;;    :else (let [])))

;; (defn pad-by [g k]
;;   )

;; ;; Paddear segun el futuro del currrent pattern hata una potencia de 2

;; ;; Make an empty 2^n grid
;; (defn gempty [n]
;;   (cond
;;    (= n 0) false
;;    (= n 1) [false false false false]
;;    :else (map (constantly (gempty (dec n))) (range 4))))



;; (do (def life (iterate (comp result pad) g0)) nil)

;; (def mresult (memoize result))
;; (do (def mlife (iterate (comp mresult pad) g0)) nil)


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
(def raphael (js/Raphael 0 0 (width) (height)))
(defn clear [color]
  (-> raphael
    (.rect 0 0 (width) (height))
    (.attr "fill" color)))

(defn printg [g x y l]
  (let [m (/ l 30)]
    (-> raphael
      (.rect (+ x m) (+ y m) (- l (* 2 m)) (- l (* 2 m)))
      (.attr "fill" "#000")
      (.attr "opacity" 0.2)))
  (cond
   (= false g) nil
   (= true g) (do
                (-> raphael
                  (.rect x y l l)
                  (.attr "fill" "#ccc")
                  (.attr "stroke" "#bbb")))
   (= 4 (count g)) (do
                     (doseq [i (range 4)]
                       (let [[ix iy] ([[0 0][1 0][1 1][0 1]] i)
                             s (/ l 2)]
                         (printg (g i) (+ x (* ix s)) (+ y (* iy s)) s))))
   (= 9 (count g)) (do
                     (doseq [i (range 9)]
                       (let [[ix iy] ([[1 1][1 0][2 0][2 1][2 2][1 2][0 2][0 1][0 0]] i)
                             s (/ l 3)]
                         (printg (g i) (+ x (* ix s)) (+ y (* iy s)) s))))))


(defn draw [g]
  (clear "#1371de")
  (printg g 0 0 (l)))

(draw g0)

;; (def latestgen (nth mlife 0))
;; (draw latestgen)

;; (def gen 0)
;; (defn loop []
;;   (def latestgen (nth mlife gen))
;;   (draw latestgen)
;;   (def gen (mod (inc gen) 10)))

;; (def intervalId (js/setInterval loop 1000))
;; (js/clearInterval intervalId)
