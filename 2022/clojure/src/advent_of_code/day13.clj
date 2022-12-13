(ns advent-of-code.day13
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (->> (puzzle 13)))

#_
(def content (str/split-lines (slurp "/home/arnaudgeiser/temp/input")))

(def couples (->> content
                  (filter seq)
                  (map read-string)
                  (partition 2)))

(defn compare-couple [[c1 c2]]
  (cond
    (and (int? c1) (int? c2))
    (cond (< c1 c2) :ok
          (= c1 c2) :same
          (> c1 c2) :ko)
    (and (coll? c1) (int? c2)) (compare-couple [c1 [c2]])
    (and (int? c1) (coll? c2)) (compare-couple [[c1] c2])
    :else (let [res (->> (map vector c1 c2) (map compare-couple) (filter (partial not= :same)) (first))]
            (if (some? res)
              res
              (cond
                (> (count c1) (count c2)) :ko
                (< (count c1) (count c2)) :ok
                :else :same)))))

(->> (mapv compare-couple couples)
     (keep-indexed (fn [i x] (when (= x :ok) (inc i))))
     (reduce +))
