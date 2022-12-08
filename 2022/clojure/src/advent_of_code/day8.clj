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

(defn taller-trees [coord trees]
  (let [height (get-in content coord)]
    (loop [acc [coord]
           rem trees
           height' height]
      (if (seq rem)
        (let [neigh (first rem)
              neigh-height (get-in content neigh)
              taller? (< height' neigh-height)]
          (recur (if taller? (conj acc neigh) acc)
                 (rest rem)
                 (if taller? neigh-height height')))
        acc))))

(defn smaller-trees [coord trees]
  (let [high (get-in content coord)]
    (count
     (loop [acc #{}
            rem trees]
       (if (seq rem)
         (let [neigh (first rem)
               neigh-height (get-in content neigh)]
           (if (> high neigh-height)
             (recur (conj acc neigh)
                    (rest rem))
             (conj acc neigh)))
         acc)))))

(defn trees-lines [lookup-fn [x y :as coord]]
  (let [left   (reverse (row x (range 0 y)))
        right  (row x (range (inc y) size))
        top    (reverse (column y (range 0 x)))
        bottom (column y (range (inc x) size))]
    (mapv (partial lookup-fn coord) [left right top bottom])))

(defn observe [acc coord]
  (->> (trees-lines taller-trees coord)
       (reduce concat)
       (reduce (fn [acc coord] (assoc acc coord true)) acc)))

(def solution1
  (->> (reduce observe {} coords)
       (sort)
       (count)))

(defn tree-house-location [acc coord]
  (->> (trees-lines smaller-trees coord)
       (reduce *)
       (assoc acc coord)))

(def coords2
  (for [x (range size)
        y (range size)]
    [x y]))

(def solution2
  (->> (reduce tree-house-location {} coords2)
       (vals)
       (reduce max)))
