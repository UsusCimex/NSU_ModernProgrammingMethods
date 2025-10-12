(require '[clojure.test :as test])

; Разбиение коллекции на блоки (через take/drop)
(defn split-into-blocks
  [coll block-size]
  (lazy-seq
    (when-let [block (seq (take block-size coll))]
      (cons block (split-into-blocks (drop block-size coll) block-size)))))

; Параллельный filter (ленивый, работает с бесконечными потоками)
(defn pfilter-lazy
  ([pred coll block-size]
   (pfilter-lazy pred coll block-size :auto))
  ([pred coll block-size buffer-size]
   (let [actual-buffer (cond
                         (= buffer-size :auto)
                         (.availableProcessors (Runtime/getRuntime))
                         (= buffer-size :unlimited)
                         Integer/MAX_VALUE
                         :else
                         buffer-size)]
     (lazy-seq
       (let [blocks (take actual-buffer (split-into-blocks coll block-size))
             futures (doall (map (fn [block]
                                   (future (doall (filter pred block))))
                                 blocks))
             results (doall (map deref futures))
             rest-coll (drop (* block-size (count blocks)) coll)]
         (when (seq blocks)
           (concat (apply concat results)
                   (pfilter-lazy pred rest-coll block-size buffer-size))))))))

; Тяжёлый предикат для демонстрации
(defn heavy-predicate [n]
  (Thread/sleep 100)
  (odd? n))

; Тесты
(test/deftest test-pfilter-lazy-correctness
  (test/is (= (filter odd? (range 20))
              (doall (pfilter-lazy odd? (range 20) 5)))))

(test/deftest test-pfilter-lazy-empty
  (test/is (empty? (pfilter-lazy odd? '() 10))))

(test/deftest test-pfilter-lazy-infinite
  (let [infinite (iterate inc 0)
        result (take 10 (pfilter-lazy odd? infinite 5))]
    (test/is (= '(1 3 5 7 9 11 13 15 17 19) result))))

; Демонстрация: работа с конечными коллекциями
(def test-data (range 160))

(println "Обычный filter:")
(time (doall (filter heavy-predicate test-data)))

(println "\nЛенивый pfilter-lazy (buffer=:auto):")
(time (doall (pfilter-lazy heavy-predicate test-data 5)))

(println "\nЛенивый pfilter-lazy (buffer=:unlimited):")
(time (doall (pfilter-lazy heavy-predicate test-data 5 :unlimited)))

; Демонстрация: бесконечные потоки
(println "\n--- Бесконечный поток ---")
(def infinite-stream (iterate inc 0))

(println "Берём 50 нечётных чисел (обычный filter):")
(time (doall (take 50 (filter heavy-predicate infinite-stream))))

(println "\nБерём 50 нечётных чисел (pfilter-lazy):")
(time (doall (take 50 (pfilter-lazy heavy-predicate infinite-stream 10))))

; Демонстрация: ленивость
(println "\n--- Ленивость ---")
(def counter (atom 0))
(defn counting-pred [n]
  (swap! counter inc)
  (odd? n))

(println "Создаём ленивую последовательность из 100 элементов:")
(def lazy-result (pfilter-lazy counting-pred (range 100) 10 1))
(println "Вызовов предиката:" @counter)

(println "\nБерём только 10 элементов:")
(reset! counter 0)
(doall (take 10 lazy-result))
(println "Вызовов предиката:" @counter)

(test/run-tests)
