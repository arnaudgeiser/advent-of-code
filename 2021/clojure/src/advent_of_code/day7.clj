(ns advent-of-code.day7
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 7))

(def numbers (->> (mapcat #(str/split % #",") content)
                  (map #(Integer/parseInt %))))

(defn solve1 []
  (->> (distinct numbers)
       (mapv (fn [number]
               (reduce (fn [acc n] (+ acc (Math/abs (- n number)))) 0 numbers)))
       (apply min)))

(solve1)

(defn consumption
  ([x acc] (if (zero? x) acc (recur (dec x) (+ acc x))))
  ([x] (consumption x 0)))

(defn solve2 []
  (let [memo-consumption (memoize consumption)]
    (->> (range (apply min numbers) (apply max numbers))
         (mapv (fn [number]
                 (reduce (fn [acc n]
                           (+ acc (memo-consumption (Math/abs (- n number)))))
                         0
                         numbers)))
         (apply min))))

(solve2)
