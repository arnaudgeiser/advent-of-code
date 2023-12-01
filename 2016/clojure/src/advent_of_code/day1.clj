(ns advent-of-code.day1
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 1))

(def mapping
  {[\R \N] \E
   [\R \E] \S
   [\R \S] \W
   [\R \W] \N
   [\L \N] \W
   [\L \W] \S
   [\L \S] \E
   [\L \E] \N})

(def solution1
  (->> (str/split (first content) #", ")
       (map #((juxt first (comp parse-long (fn [s] (subs s 1)))) %))
       (reduce (fn [acc [dir nb]]
                 (let [dir' (mapping [dir (:dir acc)])]
                   (-> acc
                       (assoc :dir dir')
                       (update-in [:moves dir'] (fnil + 0) nb))))
               {:dir \N
                :moves {}})
       :moves
       (#(+ (Math/abs (- (get % \E 0) (get % \W 0)))
            (Math/abs (- (get % \N 0) (get % \S 0)))))))

(defn new-position [[x y] dir]
  (case dir
    \N [(inc x) y]
    \S [(dec x) y]
    \W [x (inc y)]
    \E [x (dec y)]))

(def solution2
  (->> (str/split (first content) #", ")
       (map #((juxt first (comp parse-long (fn [s] (subs s 1)))) %))
       (reduce (fn [acc [dir nb]]
                 (let [dir' (mapping [dir (:dir acc)])
                       res (reduce (fn [acc _]
                                     (let [position' (new-position (:position acc) dir')]
                                       (if ((:visited acc) position')
                                         (reduced position')
                                         (-> acc
                                             (assoc :dir dir')
                                             (assoc :position position')
                                             (update :visited conj position')))))
                                   acc
                                   (range nb))]
                   (if (vector? res)
                     (reduced res)
                     res)))
               {:dir \N
                :position [0 0]
                :visited #{}})
       (reduce +)))
