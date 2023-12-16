(ns advent-of-code.day14
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 14))

#_(def content ["O....#...."
                "O.OO#....#"
                ".....##..."
                "OO.#O....O"
                ".O.....O#."
                "O.#..O.#.#"
                "..O..#O..O"
                ".......O.."
                "#....###.."
                "#OO..#...."])

(def content (mapv #(mapv char %) content))

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

(def cycle [:north :west :south :east])
(def cycles (mapcat identity (repeat cycle)))

(last (take 100001 cycles))

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

(def cycle [:north :west :south :east])

(defn do-cycle [map]
  (reduce (fn [map' dir] (move map' dir)) map cycle))

(defn learn [map]
  (loop [curr map
         knows-to-index {}
         index 0]
    (let [curr' (do-cycle curr)]
      (if (seq knows-to-index)
        (if-let [idx (knows-to-index curr')]
          [(- index idx) idx]
          (recur curr'
                 (assoc knows-to-index curr' index)
                 (inc index)))
        (recur curr'
               (assoc knows-to-index curr' index)
               (inc index))))))

#_#_#_(let [[nb-cycles after] (learn content)]
        (+ after (rem (- 1000000000 after) nb-cycles)))

    (defn solve []
      (reduce (fn [acc v] (do-cycle acc)) content (range 109)))

  (let [solved (solve)
        cnt (count solved)]
    (->> (map #(* (- cnt %) (count (filter (fn [v] (= v \O)) (nth solved %)))) (range cnt))
         (reduce +)))
