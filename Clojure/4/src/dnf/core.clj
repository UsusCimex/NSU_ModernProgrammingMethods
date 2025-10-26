(ns dnf.core
  "Базовые структуры данных для булевых выражений")

(defn const
  "Создаёт булеву константу (true/false)"
  [value]
  {:type :const :value (boolean value)})

(defn variable
  "Создаёт переменную с заданным именем"
  [name]
  {:type :var :name name})

(defmulti substitute
  "Подставляет значения переменных в выражение"
  (fn [expr _] (:type expr)))

(defmethod substitute :const [expr _] expr)

(defmethod substitute :var [expr bindings]
  (if (contains? bindings (:name expr))
    (const (get bindings (:name expr)))
    expr))

(defmethod substitute :op [expr bindings]
  (let [new-args (mapv #(substitute % bindings) (:args expr))]
    (assoc expr :args new-args)))

(defn expr-to-string
  "Преобразует выражение в строку для отладки"
  [expr]
  (cond
    (= (:type expr) :const) (str (:value expr))
    (= (:type expr) :var) (str (:name expr))
    (= (:op expr) :not) (str "!" (expr-to-string (first (:args expr))))
    (= (:op expr) :and) (str "(" (clojure.string/join " & " (map expr-to-string (:args expr))) ")")
    (= (:op expr) :or) (str "(" (clojure.string/join " | " (map expr-to-string (:args expr))) ")")
    (= (:op expr) :implies) (str "(" (expr-to-string (first (:args expr))) " -> " (expr-to-string (second (:args expr))) ")")
    (= (:op expr) :xor) (str "(" (expr-to-string (first (:args expr))) " ⊕ " (expr-to-string (second (:args expr))) ")")
    (= (:op expr) :nor) (str "NOR(" (clojure.string/join ", " (map expr-to-string (:args expr))) ")")
    :else (str expr)))
