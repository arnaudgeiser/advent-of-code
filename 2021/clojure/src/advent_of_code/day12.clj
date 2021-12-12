(ns advent-of-code.day12
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]
            [clojure.set :as set]))

(def content (puzzle 12))

(defn split-paths [input]
  (map #(str/split % #"-") input))

(defn build-cavern [paths]
  (reduce (fn [acc [s e]]
            (-> acc
                (update s (fnil conj []) e)
                (update e (fnil conj []) s)))
          {}
          paths))

(defn small-cave? [node]
  (= node (.toLowerCase node)))

(defn remove-small-caves [path possible-caves]
  (let [small-caves (set (filter small-cave? path))]
    (set/difference (set possible-caves) small-caves)))

(defn remove-small-caves-twice [path possible-caves]
  (let [small-caves (filter small-cave? path)
        two-small-caves (contains? (set/map-invert (frequencies small-caves)) 2)]
    (disj
     (if two-small-caves
       (set/difference (set possible-caves) (set small-caves))
       (set possible-caves))
     "start")))

(defn solver [eligible-caves-fn path paths cavern]
  (if (= (last path) "end")
    (conj paths path)
    (let [possible-caves (get cavern (last path))
          eligible-caves (eligible-caves-fn path possible-caves)]
      (if (empty? eligible-caves)
        paths
        (reduce (fn [paths cave]
                  (solver eligible-caves-fn (conj path cave) paths cavern))
                paths
                eligible-caves)))))

(defn solve [eligible-caves-fn]
  (->> content
       split-paths
       build-cavern
       (solver eligible-caves-fn ["start"] #{})
       count))

(def solve1 (partial solve remove-small-caves))
(def solve2 (partial solve remove-small-caves-twice))

(solve1)
(solve2)
