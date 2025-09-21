(defn add-letter [abc word]
  (map (fn [ch] (cons ch word))
       (filter (fn [ch] (not= ch (first word))) abc)))

(defn gen [abc n]
  (let [single-letters (map (fn [ch] (list ch)) abc)]
    (map (fn [w] (apply str w))
         (reduce (fn [words _]
                   (reduce concat (map (fn [w] (add-letter abc w)) words)))
                 single-letters
                 (range (dec n))))))

(println (gen ["a" "b" "c"] 2))
; => ("ba" "ca" "ab" "cb" "ac" "bc")

(println (gen ["a" "b" "c"] 3))
; => ("aba" "cba" "aca" "bca" "bab" "cab" "acb" "bcb" "bac" "cac" "abc" "cbc")
