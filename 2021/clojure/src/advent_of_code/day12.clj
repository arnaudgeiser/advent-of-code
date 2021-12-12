(ns advent-of-code.day12
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]
            [clojure.set :as set]))

(def content (puzzle 12))

(def input
  ["start-A"
   "start-b"
   "A-c"
   "A-b"
   "b-d"
   "A-end"
   "b-end"])


(defn paths [input]
  (map #(str/split % #"-") input))

(defn build-graph [paths]
  (reduce (fn [acc [s e]] (-> acc
                              (update s (fnil conj []) e)
                              (update e (fnil conj []) s))) {} paths))

(def graph (->> content
                paths
                build-graph))

(defn small-cave? [node]
  (= node (.toLowerCase node)))

(defn eligible-paths [path possible-paths]
  (let [small-caves (set (filter small-cave? path))]
    (set/difference (set possible-paths) small-caves)))

(defn solver [path graph [paths visited :as state]]
  (if (= (last path) "end")
    state
    (let [possible-paths (get graph (last path))
          eligible-paths (eligible-paths path possible-paths)]
      (if (empty? eligible-paths)
        state
        (reduce (fn [[paths visited :as acc] node]
                  (solver (conj path node) graph
                          [(conj paths (conj path node))
                           (set (if (small-cave? node) (conj visited node) visited))]))
                state
                eligible-paths)))))

(->> (solver ["start"] graph [#{} #{["start"]}])
     first
     (filter #(= (last %) "end"))
     count)

(contains? (set/map-invert (frequencies [1 1 2 3 4 4])) 2)

(defn eligible-paths2 [path possible-paths]
  (let [small-caves (filter small-cave? path)
        two-small-caves (contains? (set/map-invert (frequencies small-caves)) 2)]
    (if two-small-caves
      (set/difference (set possible-paths) (set (conj small-caves "start")))
      (disj (set possible-paths) "start"))))

(defn solver [path graph [paths visited :as state]]
  (if (= (last path) "end")
    state
    (let [possible-paths (get graph (last path))
          eligible-paths (eligible-paths2 path possible-paths)]
      (if (empty? eligible-paths)
        state
        (reduce (fn [[paths visited :as acc] node]
                  (solver (conj path node) graph
                          [(conj paths (conj path node))
                           (set (if (small-cave? node) (conj visited node) visited))]))
                state
                eligible-paths)))))

(->> (solver ["start"] graph [#{} #{["start"]}])
     first
     (filter #(= (last %) "end"))
     count)
