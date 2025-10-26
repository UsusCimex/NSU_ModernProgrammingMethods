(ns dnf.transformation
  "Приведение булевых выражений к ДНФ"
  (:require [dnf.operations :as ops]
            [dnf.simplify :as simp]))

(defn eliminate-implies
  "Устраняет импликацию: A -> B = !A | B"
  [expr]
  (cond
    (not= (:type expr) :op) expr
    (= (:op expr) :implies)
    (let [[a b] (map eliminate-implies (:args expr))]
      (ops/bool-or (ops/bool-not a) b))
    :else
    (assoc expr :args (mapv eliminate-implies (:args expr)))))

(defn eliminate-xor
  "Устраняет XOR: A ⊕ B = (A | B) & (!A | !B)"
  [expr]
  (cond
    (not= (:type expr) :op) expr
    (= (:op expr) :xor)
    (let [[a b] (map eliminate-xor (:args expr))]
      (ops/bool-and (ops/bool-or a b) (ops/bool-or (ops/bool-not a) (ops/bool-not b))))
    :else
    (assoc expr :args (mapv eliminate-xor (:args expr)))))

(defn eliminate-nor
  "Устраняет NOR: NOR(A, B, ...) = !(A | B | ...)"
  [expr]
  (cond
    (not= (:type expr) :op) expr
    (= (:op expr) :nor)
    (ops/bool-not (apply ops/bool-or (map eliminate-nor (:args expr))))
    :else
    (assoc expr :args (mapv eliminate-nor (:args expr)))))

(defn push-not-inward
  "Проталкивает отрицание внутрь по законам де Моргана"
  [expr]
  (cond
    (not= (:type expr) :op) expr

    (and (= (:op expr) :not)
         (let [inner (first (:args expr))]
           (and (= (:type inner) :op) (= (:op inner) :not))))
    (push-not-inward (first (:args (first (:args expr)))))

    (and (= (:op expr) :not)
         (let [inner (first (:args expr))]
           (and (= (:type inner) :op) (= (:op inner) :and))))
    (let [inner-args (:args (first (:args expr)))]
      (apply ops/bool-or (map (comp push-not-inward ops/bool-not) inner-args)))

    (and (= (:op expr) :not)
         (let [inner (first (:args expr))]
           (and (= (:type inner) :op) (= (:op inner) :or))))
    (let [inner-args (:args (first (:args expr)))]
      (apply ops/bool-and (map (comp push-not-inward ops/bool-not) inner-args)))

    :else
    (assoc expr :args (mapv push-not-inward (:args expr)))))

(defn distribute-and-over-or
  "Распределяет AND над OR: A & (B | C) = (A & B) | (A & C)"
  [expr]
  (cond
    (not= (:type expr) :op) expr

    (= (:op expr) :and)
    (let [args (mapv distribute-and-over-or (:args expr))
          or-args (filter #(and (= (:type %) :op) (= (:op %) :or)) args)
          other-args (remove #(and (= (:type %) :op) (= (:op %) :or)) args)]
      (if (empty? or-args)
        (apply ops/bool-and args)
        (let [or-expr (first or-args)
              rest-args (concat (rest or-args) other-args)
              distributed (mapv (fn [or-arg]
                                  (distribute-and-over-or
                                   (apply ops/bool-and (cons or-arg rest-args))))
                                (:args or-expr))]
          (apply ops/bool-or distributed))))

    :else
    (assoc expr :args (mapv distribute-and-over-or (:args expr)))))

(defn to-dnf
  "Приводит выражение к ДНФ (дизъюнктивной нормальной форме)"
  [expr]
  (-> expr
      eliminate-implies
      eliminate-xor
      eliminate-nor
      push-not-inward
      distribute-and-over-or
      simp/simplify))
