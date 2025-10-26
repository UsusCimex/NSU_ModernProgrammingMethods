(ns dnf.main
  "Главная программа с примерами использования"
  (:require [dnf.core :as core]
            [dnf.operations :as ops]
            [dnf.simplify :as simp]
            [dnf.transformation :as trans]))

(defn -main
  "Демонстрация работы с булевыми выражениями"
  [& args]
  (println "Демонстрация работы с булевыми выражениями\n")

  ; Простая импликация
  (let [x (core/variable :x)
        y (core/variable :y)
        expr (ops/bool-implies x y)]
    (println "Импликация")
    (println "Выражение:" (core/expr-to-string expr))
    (println "ДНФ:" (core/expr-to-string (trans/to-dnf expr)))
    (println))

  ; Закон де Моргана
  (let [x (core/variable :x)
        y (core/variable :y)
        expr (ops/bool-not (ops/bool-and x y))]
    (println "Закон де Моргана !(x & y)")
    (println "Выражение:" (core/expr-to-string expr))
    (println "ДНФ:" (core/expr-to-string (trans/to-dnf expr)))
    (println))

  ; Сложное выражение
  (let [x (core/variable :x)
        y (core/variable :y)
        z (core/variable :z)
        expr (ops/bool-and (ops/bool-or x y) (ops/bool-or (ops/bool-not x) z))]
    (println "(x | y) & (!x | z)")
    (println "Выражение:" (core/expr-to-string expr))
    (println "ДНФ:" (core/expr-to-string (trans/to-dnf expr)))
    (println))

  ; Подстановка значений
  (let [x (core/variable :x)
        y (core/variable :y)
        expr (ops/bool-implies x y)
        subst (core/substitute expr {:x true})
        result (simp/simplify (trans/to-dnf subst))]
    (println "Подстановка x=true в (x -> y)")
    (println "Исходное:" (core/expr-to-string expr))
    (println "После подстановки:" (core/expr-to-string subst))
    (println "ДНФ упрощённое:" (core/expr-to-string result))
    (println))

  ; XOR
  (let [x (core/variable :x)
        y (core/variable :y)
        expr (ops/bool-xor x y)]
    (println "XOR (x ? y)")
    (println "Выражение:" (core/expr-to-string expr))
    (println "ДНФ:" (core/expr-to-string (trans/to-dnf expr)))
    (println))

  ; NOR (стрелка Пирса)
  (let [x (core/variable :x)
        y (core/variable :y)
        expr (ops/bool-nor x y)]
    (println "NOR(x, y) = !(A|B)")
    (println "Выражение:" (core/expr-to-string expr))
    (println "ДНФ:" (core/expr-to-string (trans/to-dnf expr)))
    (println))

  ; Вычисление с конкретными значениями
  (let [a (core/variable :a)
        b (core/variable :b)
        c (core/variable :c)
        expr (ops/bool-or (ops/bool-and a b) (ops/bool-and (ops/bool-not a) c))
        result (simp/simplify (core/substitute expr {:a true :b false :c true}))]
    (println "Вычисление (a & b) | (!a & c) при a=true, b=false, c=true")
    (println "Выражение:" (core/expr-to-string expr))
    (println "Результат:" (core/expr-to-string result))
    (println)))
