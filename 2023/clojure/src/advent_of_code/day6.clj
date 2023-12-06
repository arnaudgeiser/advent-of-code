(ns advent-of-code.day5
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def races [[7 9] [15 40] [30 200]])
(def races [[46 347] [82 1522] [84 1406] [79 1471]])

(defn possible [race]
  (->> (map #(* % (- (first race) %)) (range (first race)))
       (filter #(> % (last race)))
       (count)))

(->> (map possible races)
     (reduce *))
