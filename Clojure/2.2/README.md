Численное интегрирование методом трапеций через поток
(defn integral [f h]
  (let [xs (iterate #(+ % h) 0.0)
        fs (map f xs)
        deltas (map (fn [f1 f2] (* 0.5 h (+ f1 f2))) fs (rest fs))
        integrals (reductions + 0 deltas)]
    (fn [x]
      (let [n (int (/ x h))]
        (nth integrals n)))))