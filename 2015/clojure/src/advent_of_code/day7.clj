(ns advent-of-code.day7
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 7))

(def content ["123 -> x"
              "456 -> y"
              "x AND y -> d"
              "x OR y -> e"
              "x LSHIFT 2 -> f"
              "y RSHIFT 2 -> g"
              "NOT x -> h"
              "NOT y -> i"])

(defn parse-line [s]
  (->> (str/split s #" -> ")
       (map #(str/split % #" "))))

(defn action->fn [s]
  (case s
    "AND" bit-and
    "OR" bit-or
    "NOT" (partial bit-and-not 0xFFFF)
    "LSHIFT" bit-shift-left
    "RSHIFT" bit-shift-right))

(defn value [s]
  (try (parse-long s)))

(defn handle-line [m [l [wire]]]
  (condp = (count l)
    1
    (update m wire (fnil + 0) (parse-long (first l)))
    2
    (let [[l1 l2] l
          f (action->fn l1)]
      (assoc m wire (f (m l2))))
    3
    (let [[l1 l2 l3] l
          f (action->fn l2)]
      (assoc m wire (f (m l1) (or (m l3) (parse-long l3)))))
    :else
    m))

(->> content
     (map parse-line)
     (reduce handle-line {}))
