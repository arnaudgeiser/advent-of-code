(ns advent-of-code.day3
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 3))

(def solution1
  (->> (re-seq #"mul\(([0-9]{1,3})*,([0-9]{1,3})*\)" (apply str content))
       (map rest)
       (map (partial map parse-long))
       (map (partial reduce *))
       (reduce +)))

(def solution2
  (->> (re-seq #"((mul)\(([0-9]{1,3})*,([0-9]{1,3})*\))|(do\(\))|(don't\(\))" (apply str content))
       (reduce (fn [acc [action _ _ n1 n2]]
                 (cond
                   (str/includes? action "mul") (if (:enabled? acc)
                                                  (update acc :sum + (* (parse-long n1) (parse-long n2)))
                                                  acc)
                   (str/includes? action "don't") (assoc acc :enabled? false)
                   (str/includes? action "do") (assoc acc :enabled? true)))
               {:enabled? true :sum 0})
       :sum))

(prn solution1)
(prn solution2)
