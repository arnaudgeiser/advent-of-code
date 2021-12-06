(ns advent-of-code.day5
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 5))

(def content' (->> content
                   (map #(str/replace % #" -> " ","))
                   (map #(map (fn [e] (Integer/parseInt e)) (str/split % #",")))))

(def initial
  (mapv (constantly (vec (repeat 1000 0))) (range 1000)))

(defn update-venture [acc x y]
  (let [row (nth acc x)
        val (nth row y)
        col (assoc row y (inc val))]
    (assoc acc x col)))

(defn range* [x1 x2]
  (if (> x1 x2)
    (range x2 (inc x1))
    (reverse (range x1 (inc x2)))))

(defn venture []
  (reduce
   (fn [acc [x1 y1 x2 y2]]
     (cond (= x1 x2)
           (reduce (fn [acc y] (update-venture acc x1 y)) acc (range* y1 y2))

           (= y1 y2)
           (reduce (fn [a x] (update-venture a x y1)) acc (range* x1 x2))

           :else
           (reduce (fn [a [x y]] (update-venture a x y)) acc
                   (map list (range* x1 x2) (range* y1 y2)))))
   initial
   content'))

(count (filter #(> % 1) (mapcat identity (venture))))
