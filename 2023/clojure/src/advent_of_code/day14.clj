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

(defn determine-position [map [x y]]
  (loop [x x]
    (let [v (get-in map [(dec x) y])]
      (if (or (nil? v) (#{\# \O} v))
        [x y]
        (recur (dec x))))))

(defn move [map]
  (reduce (fn [acc coord]
            (let [v (get-in map coord)]
              (if (= v \O)
                (do
                  (prn coord (determine-position acc coord))
                  (-> acc
                      (assoc-in coord \.)
                      (assoc-in (determine-position acc coord) \O)))
                acc))) map coords))

(defn solve []
  (move content))

(let [solved (solve)
      cnt (count solved)]
  (->> (map #(* (- cnt %) (count (filter (fn [v] (= v \O)) (nth solved %)))) (range cnt))
       (reduce +)))
