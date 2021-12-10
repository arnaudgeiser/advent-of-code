(ns advent-of-code.day9
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 9))

(def deltas [[0 1] [1 0] [-1 0] [0 -1]])

(defn neighbors [[x1 y1]]
  (->> deltas
       (map (fn [[x2 y2]] [(+ x1 x2) (+ y1 y2)]))
       (filter #(every? nat-int? %))))

(defn matrix [line]
  (->> (str/split line #"")
       (mapv #(Integer/parseInt %))))

(defn map-neighbors [coll]
  (for [i (range (count coll))
        j (range (count (nth coll i)))]
    (let [value (get-in coll [i j])
          neigh (neighbors [i j])
          values (filter some? (map #(get-in coll %) neigh))]
      [value values])))

(defn solve1 []
  (->> content
       (mapv matrix)
       (map-neighbors)
       (filter (fn [[v values]] (every? #(< v %) values)))
       (map (comp inc first))
       (reduce +)))

(solve1)

(defn map-neighbors2 [coll]
  (for [i (range (count coll))
        j (range (count (nth coll i)))]
    [i j]))

(defn neighbors2 [[x1 y1]]
  (->> deltas
       (map (fn [[x2 y2]] [(+ x1 x2) (+ y1 y2)]))
       (filter #(every? nat-int? %))))

(defn explore [matrix position]
  (let [neigh (neighbors2 position)
        eligible? #(< (get-in matrix position) (get-in matrix % 9) 9)]
    (->> neigh
         (filter eligible?)
         (mapcat (partial explore matrix))
         (reduce conj #{position}))))

(defn solve2 []
  (let [matrix (mapv matrix content)]
    (->> matrix
         (map-neighbors2)
         (map (partial explore matrix))
         (map count)
         (sort >)
         (take 3)
         (apply *))))

(solve2)
