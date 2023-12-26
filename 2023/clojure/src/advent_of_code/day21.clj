(ns advent-of-code.day21
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 21))

#_(def content ["..........."
                ".....###.#."
                ".###.##..#."
                "..#.#...#.."
                "....#.#...."
                ".##..S####."
                ".##..#...#."
                ".......##.."
                ".##.#.####."
                ".##..##.##."
                "..........."])

(def start [(/ (dec (count content)) 2) (/ (dec (count (first content))) 2)])

(def deltas [[0 1] [0 -1] [1 0] [-1 0]])

(defn garden-plots [point]
  (for [d deltas
        :let [point' (mapv + d point)]
        :when (#{\. \S} (get-in content point'))]
    point'))

(count (nth (iterate #(set (mapcat garden-plots %)) [start]) 64))
