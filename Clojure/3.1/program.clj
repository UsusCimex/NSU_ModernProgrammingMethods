(require '[clojure.test :as test])

; Разбиение коллекции на блоки (через take/drop)
(defn split-into-blocks
  [coll block-size]
  (when (not-empty coll)
    (cons (take block-size coll)
          (lazy-seq (split-into-blocks (drop block-size coll) block-size)))))

; Параллельный filter (не ленивый)
(defn pfilter
  [pred coll block-size]
  (let [blocks (doall (split-into-blocks coll block-size))
        futures (doall (map (fn [block]
                              (future (doall (filter pred block))))
                            blocks))
        results (doall (map deref futures))]
    (apply concat results)))

; Тяжёлый предикат для демонстрации
(defn heavy-predicate [n]
  (Thread/sleep 10)
  (odd? n))

; Тесты
(test/deftest test-split-into-blocks
  (test/is (= [[0 1 2] [3 4 5] [6 7 8] [9]]
              (map vec (split-into-blocks (range 10) 3)))))

(test/deftest test-pfilter-correctness
  (test/is (= (filter odd? (range 20))
              (pfilter odd? (range 20) 5))))

(test/deftest test-pfilter-empty
  (test/is (= '() (pfilter odd? '() 10))))

; Демонстрация
(def test-data (range 80))

(println "Обычный filter:")
(time (doall (filter heavy-predicate test-data)))

(println "\nПараллельный pfilter (block-size=10):")
(time (pfilter heavy-predicate test-data 10))

(println "\nПараллельный pfilter (block-size=20):")
(time (pfilter heavy-predicate test-data 20))

(test/run-tests)
