(ns advent-of-code.day14
  (:require [advent-of-code.core :refer [puzzle]]))

(def content (mapv #(mapv char %) (puzzle 14)))

(def coords
  (for [x (range (count content))
        y (range (count (first content)))]
    [x y]))

(defn determine-position [map [x y] [dx dy]]
  (loop [x x
         y y]
    (let [v (get-in map [(+ x dx) (+ y dy)])]
      (if (or (nil? v) (#{\# \O} v))
        [x y]
        (recur (+ x dx)
               (+ y dy))))))

(def mapping {:north [-1 0]
              :south [1 0]
              :east [0 1]
              :west [0 -1]})

(def move
  (memoize
   (fn [map dir]
     (reduce (fn [acc coord]
               (let [v (get-in map coord)]
                 (if (= v \O)
                   (-> acc
                       (assoc-in coord \.)
                       (assoc-in (determine-position acc coord (mapping dir)) \O))
                   acc)))
             map
             (if (#{:south :east} dir) (reverse coords) coords)))))

(def cycle-def1 [:north])
(def cycle-def2 [:north :west :south :east])

(defn do-cycle [map cycle-def]
  (reduce (fn [map' dir] (move map' dir)) map cycle-def))

(defn learn [map]
  (loop [curr map
         knows-to-index {}
         index 0]
    (let [curr' (do-cycle curr cycle-def2)]
      (if (seq knows-to-index)
        (if-let [idx (knows-to-index curr')]
          [(- index idx) idx]
          (recur curr'
                 (assoc knows-to-index curr' index)
                 (inc index)))
        (recur curr'
               (assoc knows-to-index curr' index)
               (inc index))))))

(defn required-cycles [world]
  (let [[nb-cycles after] (learn world)]
    (+ after (rem (- 1000000000 after) nb-cycles))))

(defn score [world]
  (let [cnt (count world)]
    (->> (map #(* (- cnt %) (count (filter (fn [v] (= v \O)) (nth world %)))) (range cnt))
         (reduce +))))

(defn solution1 [world]
  (score (do-cycle world cycle-def1)))

(defn solution2 [world]
  (let [nb-cycles (required-cycles world)
        result (nth (iterate #(do-cycle % cycle-def2) world) nb-cycles)]
    (score result)))

(solution1 content) ;; 109654
(solution2 content) ;; 94876
