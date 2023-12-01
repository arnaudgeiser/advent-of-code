(ns advent-of-code.day9
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 9))

(def deltas [[0 1] [1 0] [-1 0] [0 -1]])

(defn neighbors [[x1 y1]]
  (map (fn [[x2 y2]] [(+ x1 x2) (+ y1 y2)]) deltas))

(defn str->ints [line]
  (->> (str/split line #"")
       (mapv #(Integer/parseInt %))))

(defn coords [coll]
  (for [i (range (count coll))
        j (range (count (nth coll i)))]
    [i j]))

(defn solve1 []
  (let [matrix (mapv str->ints content)]
    (->> matrix
         (coords)
         (filter (fn [coord]
                   (let [get-value #(get-in matrix % 9)
                         value (get-value coord)]
                     (every? (fn [n] (< value (get-value n))) (neighbors coord)))))
         (map #(get-in matrix % 9))
         (map inc)
         (reduce +))))

(solve1)

(defn explore [matrix position]
  (let [neigh (neighbors position)
        eligible? #(< (get-in matrix position) (get-in matrix % 9) 9)]
    (->> neigh
         (filter eligible?)
         (mapcat (partial explore matrix))
         (reduce conj #{position}))))

(defn solve2 []
  (let [matrix (mapv str->ints content)]
    (->> matrix
         (coords)
         (map (partial explore matrix))
         (map count)
         (sort >)
         (take 3)
         (apply *))))

(solve2)
