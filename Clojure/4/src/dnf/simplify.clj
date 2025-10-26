(ns dnf.simplify
  "Упрощение булевых выражений"
  (:require [dnf.core :as core]
            [dnf.operations :as ops]))

(defn negation-of?
  "Проверяет, является ли expr1 отрицанием expr2"
  [expr1 expr2]
  (cond
    ; expr1 = NOT expr2
    (and (= (:type expr1) :op)
         (= (:op expr1) :not)
         (= (first (:args expr1)) expr2))
    true

    ; expr2 = NOT expr1
    (and (= (:type expr2) :op)
         (= (:op expr2) :not)
         (= (first (:args expr2)) expr1))
    true

    :else false))

(defn has-contradiction?
  "Проверяет, есть ли в списке аргументов пара x и !x"
  [args]
  (some (fn [expr1]
          (some #(negation-of? expr1 %) args))
        args))

(defmulti simplify-op
  "Упрощает операции с учётом констант и правил булевой алгебры"
  (fn [op _] op))

(defmethod simplify-op :not [_ [expr]]
  (cond
    (= (:type expr) :const) (core/const (not (:value expr)))
    (and (= (:type expr) :op) (= (:op expr) :not)) (first (:args expr))
    :else (ops/bool-not expr)))

(defmethod simplify-op :and [_ args]
  (let [simplified (remove #(and (= (:type %) :const) (:value %)) args)]
    (cond
      ; Если есть false - весь AND ложен
      (some #(and (= (:type %) :const) (not (:value %))) args)
      (core/const false)

      ; Если есть x AND !x - противоречие, результат false
      (has-contradiction? simplified)
      (core/const false)

      ; Если после удаления true ничего не осталось
      (empty? simplified)
      (core/const true)

      ; Если остался один аргумент
      (= (count simplified) 1)
      (first simplified)

      ; Иначе возвращаем AND без true
      :else
      (apply ops/bool-and simplified))))

(defmethod simplify-op :or [_ args]
  (let [simplified (remove #(and (= (:type %) :const) (not (:value %))) args)]
    (cond
      ; Если есть true - весь OR истинен
      (some #(and (= (:type %) :const) (:value %)) args)
      (core/const true)

      ; Если есть x OR !x - тавтология, результат true
      (has-contradiction? simplified)
      (core/const true)

      ; Если после удаления false ничего не осталось
      (empty? simplified)
      (core/const false)

      ; Если остался один аргумент
      (= (count simplified) 1)
      (first simplified)

      ; Иначе возвращаем OR без false
      :else
      (apply ops/bool-or simplified))))

(defmethod simplify-op :implies [_ [a b]]
  (cond
    (and (= (:type a) :const) (not (:value a))) (core/const true)
    (and (= (:type a) :const) (:value a)) b
    (and (= (:type b) :const) (:value b)) (core/const true)
    (and (= (:type b) :const) (not (:value b))) (ops/bool-not a)
    :else (ops/bool-implies a b)))

(defmethod simplify-op :xor [_ [a b]]
  (cond
    (and (= (:type a) :const) (= (:type b) :const))
    (core/const (not= (:value a) (:value b)))
    (and (= (:type a) :const) (not (:value a))) b
    (and (= (:type b) :const) (not (:value b))) a
    (and (= (:type a) :const) (:value a)) (ops/bool-not b)
    (and (= (:type b) :const) (:value b)) (ops/bool-not a)
    :else (ops/bool-xor a b)))

(defmethod simplify-op :nor [_ args]
  (let [or-result (simplify-op :or args)]
    (if (= (:type or-result) :const)
      (core/const (not (:value or-result)))
      (ops/bool-not or-result))))

(defmulti simplify
  "Упрощает булево выражение"
  :type)

(defmethod simplify :const [expr] expr)
(defmethod simplify :var [expr] expr)

(defmethod simplify :op [expr]
  (let [simplified-args (mapv simplify (:args expr))]
    (simplify-op (:op expr) simplified-args)))
