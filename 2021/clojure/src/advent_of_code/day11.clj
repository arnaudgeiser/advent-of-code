(ns advent-of-code.day11
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 11))

(def height (count content))
(def width (count (first content)))

(defn str->ints [line]
  (->> (str/split line #"")
       (mapv #(Integer/parseInt %))))

(def deltas
  (for [i (range -1 2)
        j (range -1 2)]
    [i j]))

(defn neighbors [position]
  (->> deltas
       (map (fn [neighbor] (map (comp (partial reduce +) vector) position
                                neighbor)))
       (remove (fn [[x y]] (or (neg-int? y)
                               (neg-int? x)
                               (>= x height)
                               (>= y  width))))))

(defn coords [coll]
  (for [i (range (count coll))
        j (range (count (nth coll i)))]
    [i j]))

(defn increase-matrix [matrix]
  (mapv (partial mapv inc) matrix))

(defn handle-flashes [[matrix flashed :as result] position]
  (let [value (get-in matrix position)]
    (if (contains? flashed position)
      result
      (if (> value 9)
        (let [neigh (remove (partial contains? flashed) (neighbors position))
              inc-matrix (reduce #(update-in %1 %2 inc) matrix neigh)]
          (reduce handle-flashes [inc-matrix (conj flashed position)] neigh))
        result))))

(defn step [matrix]
  (let [increased-matrix (increase-matrix matrix)]
    (->> increased-matrix
         coords
         (reduce handle-flashes [increased-matrix #{}])
         first
         (mapv (partial mapv #(if (> % 9) 0 %))))))

(defn solve1 [matrix]
  (loop [i 100
         flashs 0
         matrix matrix]
    (if (zero? i)
      flashs
      (let [new-matrix (step matrix)]
        (recur (dec i)
               (+ flashs (count (mapcat #(filter zero? %) new-matrix)))
               new-matrix)))))

(defn solve2 [matrix]
  (loop [i 0
         matrix matrix]
    (if (every? zero? (mapcat identity matrix))
      i
      (let [new-matrix (step matrix)]
        (recur (inc i)
               new-matrix)))))

(solve1 (map str->ints content))
(solve2 (map str->ints content))
