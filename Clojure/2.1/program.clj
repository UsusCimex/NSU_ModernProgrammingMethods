(require '[clojure.test :as test])

; Численное интегрирование методом трапеций
(defn integral
  [f h]
  (let [block-size 1000
        block-sum (memoize
                   (fn [block-idx]
                     (let [start (* block-idx block-size)
                           end (+ start block-size)]
                       (loop [i start
                              acc 0.0]
                         (if (>= i end)
                           acc
                           (recur (inc i) (+ acc (f (* i h)))))))))
        partial-sum (fn [n]
                      (let [full-blocks (quot n block-size)
                            blocks-sum (loop [b 0
                                              acc 0.0]
                                         (if (>= b full-blocks)
                                           acc
                                           (recur (inc b)
                                                  (+ acc (block-sum b)))))
                            remainder-sum (let [start (* full-blocks block-size)]
                                            (loop [i start
                                                   acc 0.0]
                                              (if (>= i n)
                                                acc
                                                (recur (inc i)
                                                       (+ acc (f (* i h)))))))]
                        (+ blocks-sum remainder-sum)))
        int-fn (fn [x]
                 (let [n (int (/ x h))]
                   (if (zero? n)
                     0.0
                     (let [inner-sum (partial-sum n)
                           approx (+ (* 0.5 (f 0))
                                     inner-sum
                                     (* 0.5 (f (* n h))))
                           result (* h approx)]
                       result))))]
    int-fn))


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

(time (println "I(500) =" (F 500.0)))
(time (println "I(999) =" (F 999.0)))
(time (println "I(1000) =" (F 1000.0)))
(time (println "I(1500) =" (F 1500.0)))
(time (println "I(2000) =" (F 2000.0)))
(time (println "I(1500) =" (F 1500.0)))
(time (println "I(500) =" (F 500.0)))

(test/run-tests)