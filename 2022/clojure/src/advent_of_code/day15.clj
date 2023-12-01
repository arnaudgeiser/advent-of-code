(ns advent-of-code.day15
  (:require [advent-of-code.core :refer [puzzle ints]]
            [clojure.string :as str]))

(def content (puzzle 15))

(def content (str/split-lines (slurp "/home/arnaudgeiser/temp/input")))

(def content (->> content
                  (mapv ints)))

(reduce (fn [acc [x1 y1 x2 y2]]
          (let [distance (Math/abs (- x1 x2))])) 0 content)

content
