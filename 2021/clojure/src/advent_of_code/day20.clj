(ns advent-of-code.day20
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]
            [clojure.set :as set]))

(def content (puzzle 20))

(def mapping
  (->> content
       (take 2)
       (str/join)
       (map-indexed (fn [i v] [i v]) )
       (into {})))

(def full-map
  (->> content
       (drop 3)
       (mapv #(str/split % #""))
       (into [])))

(defn group-fn [full-map]
  (let [max-x (count full-map)
        max-y (count (first full-map))
        valid? (fn [[x y]] (and (nat-int? x) (nat-int? y) (< x max-x) (< y max-y)))]
    (fn [[x y]]
      (filterv valid? [[(dec x) (dec y)] [x (dec y)] [(inc x) (inc y)]
                       [(dec x) y] [x y] [(inc x) y]
                       [(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]]))))
(def points
  (into []
        (mapcat identity (map-indexed (fn [y row] (into [] (map-indexed (fn [x _] [x y]) row))) full-map))))

(defn symbol->binary [s]
  (if (= s "#") 1 0))

(defn group->int [full-map group]
  (->> group
       (map (partial get-in full-map))
       (map symbol->binary)
       (str/join)
       (#(BigInteger. % 2))))

(group->int full-map [[0 1] [1 0]])

(defn enhance [full-map]
  (partition 100
             (let [get-group (group-fn full-map)]
               (for [point points]
                 (let [group (get-group point)
                       int-value (group->int full-map group)
                       new-val (get mapping int-value)]
                   new-val)))))

(->> (iterate enhance full-map)
     (take 2)
     (flatten)
     (filter (partial = \#))
     (count))

(count (first full-map))
