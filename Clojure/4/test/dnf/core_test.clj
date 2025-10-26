(ns dnf.core-test
  "Тесты для всех модулей библиотеки"
  (:require [clojure.test :refer :all]
            [dnf.core :as core]
            [dnf.operations :as ops]
            [dnf.simplify :as simp]
            [dnf.transformation :as trans]))

(defn -main
  "Запуск тестов"
  [& args]
  (run-tests 'dnf.core-test))

(deftest test-basic-operations
  (let [x (core/variable :x)
        y (core/variable :y)]
    (is (= (:type x) :var))
    (is (= (:type (core/const true)) :const))
    (is (= (:type (ops/bool-and x y)) :op))))

(deftest test-substitution
  (let [x (core/variable :x)
        y (core/variable :y)
        expr (ops/bool-and x y)]
    (is (= (core/substitute x {:x true}) (core/const true)))
    (is (= (core/substitute expr {:x true, :y false})
           (ops/bool-and (core/const true) (core/const false))))))

(deftest test-simplification
  (let [x (core/variable :x)]
    (is (= (simp/simplify (ops/bool-and x (core/const true))) x))
    (is (= (simp/simplify (ops/bool-and x (core/const false))) (core/const false)))
    (is (= (simp/simplify (ops/bool-or x (core/const true))) (core/const true)))
    (is (= (simp/simplify (ops/bool-or x (core/const false))) x))
    (is (= (simp/simplify (ops/bool-not (ops/bool-not x))) x))))

(deftest test-implies-elimination
  (let [x (core/variable :x)
        y (core/variable :y)
        expr (ops/bool-implies x y)]
    (is (= (:op (trans/eliminate-implies expr)) :or))))

(deftest test-de-morgan
  (let [x (core/variable :x)
        y (core/variable :y)
        expr (ops/bool-not (ops/bool-and x y))
        result (trans/push-not-inward expr)]
    (is (= (:op result) :or))))

(deftest test-dnf-simple
  (let [x (core/variable :x)
        y (core/variable :y)
        expr (ops/bool-implies x y)
        dnf (trans/to-dnf expr)]
    (is (or (= (:op dnf) :or) (= (:type dnf) :var)))))

(deftest test-substitution-with-dnf
  (let [x (core/variable :x)
        y (core/variable :y)
        expr (ops/bool-implies x y)
        subst (core/substitute expr {:x true})
        dnf (trans/to-dnf subst)
        result (simp/simplify dnf)]
    (is (or (= result y) (and (= (:type result) :const) (:value result))))))

(deftest test-xor-operation
  (let [x (core/variable :x)
        y (core/variable :y)
        expr (ops/bool-xor x y)]
    (is (= (:op expr) :xor))
    (is (= (simp/simplify (core/substitute expr {:x true :y true})) (core/const false)))
    (is (= (simp/simplify (core/substitute expr {:x true :y false})) (core/const true)))))

(deftest test-nor-operation
  (let [x (core/variable :x)
        y (core/variable :y)
        expr (ops/bool-nor x y)]
    (is (= (:op expr) :nor))
    (is (= (simp/simplify (core/substitute expr {:x false :y false})) (core/const true)))
    (is (= (simp/simplify (core/substitute expr {:x true :y false})) (core/const false)))))

(deftest test-contradiction-in-and
  "x AND !x всегда false"
  (let [x (core/variable :x)
        expr (ops/bool-and x (ops/bool-not x))]
    (is (= (simp/simplify expr) (core/const false)))))

(deftest test-tautology-in-or
  "x OR !x всегда true"
  (let [x (core/variable :x)
        expr (ops/bool-or x (ops/bool-not x))]
    (is (= (simp/simplify expr) (core/const true)))))

(deftest test-contradiction-with-other-terms
  "x AND y AND !x = false"
  (let [x (core/variable :x)
        y (core/variable :y)
        expr (ops/bool-and x y (ops/bool-not x))]
    (is (= (simp/simplify expr) (core/const false)))))

(deftest test-tautology-with-other-terms
  "x OR y OR !x = true"
  (let [x (core/variable :x)
        y (core/variable :y)
        expr (ops/bool-or x y (ops/bool-not x))]
    (is (= (simp/simplify expr) (core/const true)))))
