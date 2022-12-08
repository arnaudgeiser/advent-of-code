(ns advent-of-code.day8
  (:require [advent-of-code.core :refer [puzzle]]))

(def content (->> (puzzle 8)
                  (mapv (partial mapv (comp parse-long str)))))

(def size (count content))

(def coords (->> (concat
                  (mapv #(vector 0 %) (range size))
                  (mapv #(vector % 0) (range size))
                  (mapv #(vector % (dec size)) (range size))
                  (mapv #(vector (dec size) %) (range size)))))

(defn row [x range] (mapv #(vector x %) range))

(defn column [y range] (mapv #(vector % y) range))

(defn tree-coords [coll coord]
  (let [high (get-in content coord)]
    (loop [acc [coord]
           coll' coll
           high' high]
      (if (seq coll')
        (let [neigh (first coll')
              neigh-height (get-in content neigh)
              taller? (< high' neigh-height)]
          (recur (if taller? (conj acc neigh) acc)
                 (rest coll')
                 (if taller? neigh-height high')))
        acc))))

(defn observe [acc [x y :as coord]]
  (let [left  (reverse (row x (range 0 y)))
        right (row x (range (inc y) size))
        top (reverse (column y (range 0 x)))
        bottom (column y (range (inc x) size))]
    (->> (concat (tree-coords right coord)
                 (tree-coords left coord)
                 (tree-coords top coord)
                 (tree-coords bottom coord))
         (reduce (fn [acc coord] (assoc acc coord true)) acc))))

(->> (reduce observe {} coords)
     (sort)
     (count))
