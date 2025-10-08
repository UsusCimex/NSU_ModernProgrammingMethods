; my-map через рекурсию
(defn my-map [f coll]
  (if (empty? coll)
    (list)
    (cons (f (first coll))
          (my-map f (rest coll)))))

; my-filter через рекурсию
(defn my-filter [pred coll]
  (if (empty? coll)
    (list)
    (if (pred (first coll))
      (cons (first coll) (my-filter pred (rest coll)))
      (my-filter pred (rest coll)))))

(println (my-map inc (list 1 2 3 4)))
; => (2 3 4 5)

(println (my-map (fn [x] (* x x)) (list 1 2 3 4)))
; => (1 4 9 16)

(println (my-filter (fn [n] (= 0 (mod n 3))) (range 1 21)))
; => (3 6 9 12 15 18)