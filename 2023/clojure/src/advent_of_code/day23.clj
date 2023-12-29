(ns advent-of-code.day23
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 23))

#_
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

(defn dfs [visited path]
  (loop [neighs (remove visited (neighbors (last path)))
         paths [path]]
    (if (seq neighs)
      (recur (rest neighs)
             (concat paths (dfs (conj visited (first neighs)) (conj path (first neighs)))))
      paths))
  #_
 (loop [visited visited
        path path]
   (for [n (remove visited (neighbors (last path)))]
     (dfs (conj visited n) (conj path n)))))

(->> (dfs #{} [start])
     (filter #(= end (last %)))
     (map (comp dec count))
     (apply max))
