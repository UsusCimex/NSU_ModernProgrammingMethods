(defn make-words
  ([abc] (make-words abc (list)))
  ([abc acc]
   (if (> (count abc) 0)
     (recur (rest abc) (cons (list (first abc)) acc))
     acc)))

(defn add-letter
  ([abc word] (add-letter abc word (list)))
  ([abc word acc]
   (if (> (count abc) 0)
     (let [ch (first abc)]
       (if (= ch (first word))
         (recur (rest abc) word acc)
         (recur (rest abc) word (cons (cons ch word) acc))))
     acc)))

(defn grow
  ([abc words] (grow abc words (list)))
  ([abc words acc]
   (if (> (count words) 0)
     (recur abc (rest words) (concat acc (add-letter abc (first words))))
     acc)))

(defn build
  ([abc n]
   (cond
     (= n 0) (list (list))
     (= n 1) (make-words abc)
     :else  (build abc n (make-words abc))))
  ([abc n acc]
   (if (= n 1)
     acc
     (recur abc (dec n) (grow abc acc)))))

(defn to-list-strings
  ([words] (to-list-strings words (list)))
  ([words acc]
   (if (> (count words) 0)
     (recur (rest words) (cons (apply str (first words)) acc))
     acc)))

(defn gen [abc n]
  (to-list-strings (build abc n)))

(println (gen (list "a" "b" "c") 2))
; => (ba ca ab cb ac bc)

(println (gen (list "a" "b" "c") 3))
; => (aba cba aca bca bab cab acb bcb bac cac abc cbc)