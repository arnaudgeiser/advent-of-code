(ns advent-of-code.day23
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 23))

(def content
  ["#.#####################"
   "#.......#########...###"
   "#######.#########.#.###"
   "###.....#.>.>.###.#.###"
   "###v#####.#v#.###.#.###"
   "###.>...#.#.#.....#...#"
   "###v###.#.#.#########.#"
   "###...#.#.#.......#...#"
   "#####.#.#.#######.#.###"
   "#.....#.#.#.......#...#"
   "#.#####.#.#.#########v#"
   "#.#...#...#...###...>.#"
   "#.#.#v#######v###.###v#"
   "#...#.>.#...>.>.#.###.#"
   "#####v#.#.###v#.#.###.#"
   "#.....#...#...#.#.#...#"
   "#.#########.###.#.#.###"
   "#...###...#...#...#.###"
   "###.###.#.###v#####v###"
   "#...#...#.#.>.>.#.>.###"
   "#.###.###.#.###.#.#v###"
   "#.....###...###...#...#"
   "#####################.#"])

(def start [0 1])
(def max-y (dec (count content)))
(def end [max-y (first (filter #(= \. (get-in content [max-y %])) (range (count (first content)))))])

(def deltas [[1 0] [-1 0] [0 1] [0 -1]])
(def delta-mapping {\. deltas \> [[0 1]] \< [[0 -1]] \v [[1 0]] \^ [-1 0]})

(defn neighbors [[y x]]
  (for [[y' x'] (delta-mapping (get-in content [y x]))
        :let [coord [(+ y y') (+ x x')]
              v (get-in content coord)]
        :when (#{\. \> \^ \v \<} v)]
    coord))

(defn paths-part1 [start-pos]
  (loop [[[seen pos] & rem :as stack] (list [#{} start-pos])
         result []]
    (cond
      (nil? seen)
      result

      (= pos end)
      (recur rem
             (conj result (count seen)))

      :else
      (recur (concat rem (map (fn [pos'] [(conj seen pos) pos']) (remove seen (neighbors pos))))
             result))))

(defn solution1 []
  (apply max (paths-part1 start)))

(solution1) ;; 94
