(defn make-words [abc]
  (if (> (count abc) 0)
    (cons (list (first abc))
          (make-words (rest abc)))
    (list)))

(defn add-letter [abc word]
  (if (> (count abc) 0)
    (let [ch (first abc)]
      (if (= ch (first word))
        (add-letter (rest abc) word)
        (cons (cons ch word)
              (add-letter (rest abc) word))))
    (list)))

(defn grow [abc words]
  (if (> (count words) 0)
    (concat (add-letter abc (first words))
            (grow abc (rest words)))
    (list)))

(defn build [abc n]
  (if (= n 0)
    (list (list))
    (if (= n 1)
      (make-words abc)
      (grow abc (build abc (dec n))))))

(defn to-list-strings [words]
  (if (> (count words) 0)
    (cons (apply str (first words))
          (to-list-strings (rest words)))
    (list)))

(defn gen [abc n]
  (to-list-strings (build abc n)))

(println (gen (list "a" "b" "c") 2))
; => (ba ca ab cb ac bc)

(println (gen (list "a" "b" "c") 3))
; => (aba cba aca bca bab cab acb bcb bac cac abc cbc)
