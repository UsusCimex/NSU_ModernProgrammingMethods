(require '[clojure.test :as test])

; Численное интегрирование методом трапеций
(defn integral
  [f h]
  (let [int-fn (fn [x]
                 (let [n (int (/ x h))
                       xs (for [i (range 1 n)] (* i h))
                       sum (reduce + (map f xs))
                       approx (+ (* 0.5 (f 0))
                                 sum
                                 (* 0.5 (f (* n h))))
                       result (* h approx)]
                   result))]
    (memoize int-fn)))


; Тесты
(test/deftest test-integral
  (let [f (fn [t] (* t t))
        F (integral f 0.001)]
    ; Точные значения: ∫0^x t^2 dt = x^3 / 3
    (test/is (< (Math/abs (- (F 1.0) (/ 1.0 3))) 1e-3))
    (test/is (< (Math/abs (- (F 2.0) (/ 8.0 3))) 1e-3))
    (test/is (< (Math/abs (- (F 3.0) (/ 27.0 3))) 1e-3))))


; main
(println "Вычисление интеграла f(t)=t^2")
(def f (fn [t] (* t t)))
(def F (integral f 0.001))

(time (println "I(1500) =" (F 1500.0)))
(time (println "I(1000) =" (F 1000.0)))
(time (println "I(1000) =" (F 1000.0)))
(time (println "I(500) =" (F 500.0)))

(test/run-tests)