(ns advent-of-code.day20
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]
            [clojure.set :as set]))

(def content (puzzle 20))

(def mapping (->> content
                  (take 2)
                  (str/join)
                  (map-indexed (fn [i v] [i v]))
                  (into {})))

(defn full-map [content]
  (->> content
       (drop 3)
       (mapv #(str/split % #""))
       (into [])))

(defn group [[x y]]
  [[(dec x) (dec y)] [x (dec y)] [(inc x) (inc y)]
   [(dec x) y] [x y] [(inc x) y]
   [(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]])

(defn points [full-map]
  (into {}
        (mapcat identity
                (map-indexed (fn [y row]
                               (map-indexed (fn [x v] [[x y] v]) row))
                             full-map))))

(defn build-map [content]
  (let [full-map (full-map content)
        xsize (count (first full-map))
        ysize (count full-map)]
    {:xsize xsize
     :ysize ysize
     :expand 0
     :points (points full-map)}))

(defn symbol->binary [s]
  (if (= s "#") 1 0))

(defn group->int [group points]
  (->> group
       (map #(get points %))
       (map symbol->binary)
       (str/join)
       (#(BigInteger. % 2))))

(defn enhance [{:keys [xsize ysize expand points]}]
  {:xsize xsize
   :ysize ysize
   :expand (inc expand)
   :points
   (into {} (for [x (range (dec (- expand)) (inc (+ xsize expand)))
                  y (range (dec (- expand)) (inc (+ ysize expand)))]
              (let [group (group [x y])
                    int-value (group->int group points)
                    new-val (get mapping int-value)]
                [[x y] new-val])))})

(range -5 5)

(first (enhance (build-map content)))

(->> (iterate enhance (build-map content))
     (take 2)
     (last)
     :expand
     #_#_#_
     vals
     (filter (partial = \#))
     (count))

#_
(count (first full-map))
