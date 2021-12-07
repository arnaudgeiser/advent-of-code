(ns advent-of-code.day8
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 8))

(def numbers (->> (mapcat #(str/split % #",") content)
                  (map #(Integer/parseInt %))))

(defn solve [])
