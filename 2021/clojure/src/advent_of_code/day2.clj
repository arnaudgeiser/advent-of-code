(ns advent-of-code.day2
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 2))

(defn compute [[horizontal depth aim] [direction nb]]
  (let [nb' (Integer/parseInt nb)]
    (condp = direction
      "forward" [(+ horizontal nb') (+ depth (* aim nb')) aim]
      "down" [horizontal depth (+ aim nb')]
      "up"   [horizontal depth (- aim nb')])))

(->> content
     (map #(str/split % #" "))
     (reduce compute [0 0 0])
     (take 2)
     (apply *))
