(ns dnf.operations
  "Булевые операции: AND, OR, NOT, IMPLIES, XOR, NOR")

(defn bool-not
  "Создаёт отрицание выражения"
  [expr]
  {:type :op :op :not :args [expr]})

(defn bool-and
  "Создаёт конъюнкцию (AND) выражений"
  [& exprs]
  (cond
    (empty? exprs) {:type :const :value true}
    (= (count exprs) 1) (first exprs)
    :else {:type :op :op :and :args (vec exprs)}))

(defn bool-or
  "Создаёт дизъюнкцию (OR) выражений"
  [& exprs]
  (cond
    (empty? exprs) {:type :const :value false}
    (= (count exprs) 1) (first exprs)
    :else {:type :op :op :or :args (vec exprs)}))

(defn bool-implies
  "Создаёт импликацию (A -> B)"
  [a b]
  {:type :op :op :implies :args [a b]})

(defn bool-xor
  "Создаёт исключающее ИЛИ (XOR)"
  [a b]
  {:type :op :op :xor :args [a b]})

(defn bool-nor
  "Создаёт стрелку Пирса (NOR)"
  [& exprs]
  (cond
    (empty? exprs) {:type :const :value true}
    (= (count exprs) 1) (bool-not (first exprs))
    :else {:type :op :op :nor :args (vec exprs)}))
