(ns advent-of-code.day7
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (map #(str/replace % "$ " "") (puzzle 7)))

(defn str->long [s] (try (parse-long s)))

(defn cd [wd arg]
  (case arg
    "/" [arg]
    ".." (pop wd)
    (conj wd arg)))

(defn dirs [dir] (rest (reductions conj [] dir)))

(defn update-hierarchy [hierarchy wd size]
  (reduce (fn [hierarchy dir] (update hierarchy dir (fnil + 0) size)) hierarchy (dirs wd)))

(def hierarchy
  (->> (reduce (fn [{:keys [hierarchy wd] :as acc} line]
                 (let [[s1 s2] (str/split line #" ")]
                  (cond
                    (= s1 "cd")    (assoc acc :wd (cd wd s2))
                    (str->long s1) (assoc acc :hierarchy (update-hierarchy hierarchy wd (str->long s1)))
                    :else          acc)))
               {:wd [] :hierarchy {}}
               content)
       :hierarchy))

(def solution1
  (->> (vals hierarchy)
       (filter (partial > 100000))
       (reduce +)))

(def solution2
  (let [used-space (get hierarchy ["/"])]
    (->> (vals hierarchy)
         (filter (partial < (- 30000000 (- 70000000 used-space))))
         (sort)
         (first))))
