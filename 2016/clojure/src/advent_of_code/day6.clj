(ns advent-of-code.day6
  (:require [advent-of-code.core :refer [puzzle]]))

(def content (puzzle 6))

(defn init [v]
  (->> (for [x (range 1000)
             y (range 1000)]
         [[x y] v])
       (into {})))

(defn parse-line [s]
  (let [[_ a & rest] (re-find #"(turn on|turn off|toggle) ([0-9]*),([0-9]*) through ([0-9]*),([0-9]*)" s)]
    (concat [a] (map parse-long rest))))

(defn action->fn [s]
  (case s
    "turn on" (constantly true)
    "turn off" (constantly false)
    "toggle" not))

(defn handle-action [action->fn acc [a x y dx dy]]
  (let [lights (for [x' (range x (inc dx))
                     y' (range y (inc dy))]
                 [x' y'])]
   (reduce (fn [acc coll]
             (update acc coll (action->fn a))) acc lights)))

(def solution1
  (->> content
       (map parse-line)
       (reduce (partial handle-action action->fn) (init false))
       (vals)
       (filter identity)
       (count)))

(defn action->fn2 [s]
  (case s
    "turn on" inc
    "turn off" (comp (partial max 0) dec)
    "toggle" (partial + 2)))

(def solution2
  (->> content
       (map parse-line)
       (reduce (partial handle-action action->fn2) (init 0))
       (vals)
       (filter identity)
       (reduce +)))
