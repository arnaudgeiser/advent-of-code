(ns advent-of-code.day20
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 20))

(def mapping (->> (take 1 content)
                  (str/join)
                  (map-indexed (fn [i v] [i (char v)]))
                  (into {})))

(defn full-map [content]
  (->> content
       (drop 2)
       (mapv (fn [line] (map #(.charAt % 0) (str/split line #""))))
       (into [])))

(defn group [[x y]]
  [[(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]
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

(defn symbol->binary [default s]
  (cond (= s \#) 1
        (= s \.) 0
        :else default))

(defn group->int [group points default]
  (->> group
       (map #(get points %))
       (map (partial symbol->binary default))
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
                    int-value (group->int group points (if (zero? (mod expand 2)) 0 1))
                    new-val (get mapping int-value)]
                [[x y] new-val])))})

(defn solve [content nb]
  (->> (iterate enhance (build-map content))
       (take (inc nb))
       (last)
       (:points)
       vals
       (filter (partial = \#))
       (count)))

(solve content 2)
(solve content 50)
