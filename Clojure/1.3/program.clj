(defn my-map [f coll]
  (reduce
   (fn [acc x] (concat acc (list (f x))))
   (list)
   coll))

(println (my-map inc (list 1 2 3 4)))
; => (2 3 4 5)

(println (my-map (fn [x] (* x x)) (list 1 2 3 4)))
; => (1 4 9 16)

(defn my-filter [pred coll]
  (reduce
   (fn [acc x]
     (if (pred x)
       (concat acc (list x)) 
       acc))
   (list)
   coll))

(println (my-filter (fn [n] (= 0 (mod n 3))) (range 1 21)))
; => (3 6 9 12 15 18)
