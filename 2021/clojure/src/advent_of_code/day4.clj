(ns advent-of-code.day4
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.set :as set]
            [clojure.string :as str]))

(def content (puzzle 4))

(def numbers (->> (str/split (first content) #",")
                  (map (fn [e] (Integer/parseInt e)))))

(def boards (->> (rest content)
                 (remove (partial = ""))
                 (map #(map (fn [e] (Integer/parseInt e)) (remove (partial = "") (str/split % #" "))))
                 (partition-all 5)))

(defn winner-board [boards numbers]
  (first (filter (fn [board]
                   (let [winner-coll #(some (fn [coll] (set/subset? (set coll) (set numbers))) %)
                         rows board
                         columns (apply map list board)]
                     (or (winner-coll rows) (winner-coll columns))))
                 boards)))

(loop [i 1]
  (let [pulled (take i numbers)
        last (last pulled)]
    (if-let [winner-board (winner-board boards pulled)]
      (* last (reduce + (set/difference (set (mapcat identity winner-board)) (set pulled))))
      (recur (inc i)))))

(defn winner-boards [boards numbers]
  (filter (fn [board]
            (let [winner-coll #(some (fn [coll] (set/subset? (set coll) (set numbers))) %)
                  rows board
                  columns (apply map list board)]
              (or (winner-coll rows) (winner-coll columns))))
          boards))

(loop [i 1
       rem-boards boards]
  (let [pulled (take i numbers)
        last (last pulled)]
    (if (and (= 1 (count rem-boards)) (seq (winner-boards rem-boards pulled)))
      (let [last-board (first rem-boards)]
        (* last (reduce + (set/difference (set (mapcat identity last-board)) (set pulled)))))
      (let [winner-boards (winner-boards boards pulled)
            rem-boards' (remove (fn [board] (some (partial = board) winner-boards)) rem-boards)]
        (recur (inc i) rem-boards')))))
