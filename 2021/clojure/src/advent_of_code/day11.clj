(ns advent-of-code.day11
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]
            [clojure.set :as set]))

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
       (map (fn [neighbor]
              (->> neighbor
                   (map (comp (partial reduce +) vector) position))))
       (remove (fn [[x y]] (or (neg-int? y) (neg-int? x) (>= x height) (>= y  width))))))

(defn coords [coll]
  (for [i (range (count coll))
        j (range (count (nth coll i)))]
    [i j]))

(defn step1 [matrix]
  (mapv (partial mapv inc) matrix))

(defn step2 [[matrix flashed :as result] position]
  (let [value (get-in matrix position)]
    (if (contains? flashed position)
      result
      (if (> value 9)
        (let [neigh (remove (partial contains? flashed) (neighbors position))
              inc-matrix (reduce #(update-in %1 %2 inc) matrix neigh)]
          (reduce step2 [inc-matrix (conj flashed position)] neigh)
          )
        result))))

(def input ["11111"
            "19991"
            "19191"
            "19991"
            "11111"])

(defn do-step [matrix]
  (let [matrix' (step1 matrix)]
    (->> matrix'
         coords
         (reduce step2 [matrix' #{}])
         first
         (mapv (partial mapv #(if (> % 9) 0 %))))))

(defn solve1 [matrix]
  (loop [i 100
         nb-flashs 0
         matrix matrix]
    (if (zero? i)
      nb-flashs
      (let [new-matrix (do-step matrix)]
        (recur (dec i)
               (+ nb-flashs (count (mapcat #(filter zero? %) new-matrix)))
               new-matrix)))))

(def input2
  ["5483143223"
   "2745854711"
   "5264556173"
   "6141336146"
   "6357385478"
   "4167524645"
   "2176841721"
   "6882881134"
   "4846848554"
   "5283751526"]
  )

(comment (solve1 (mapv str->ints content)))
