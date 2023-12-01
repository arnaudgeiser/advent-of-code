(ns advent-of-code.day21
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]
            [clojure.set :as set]))

(def content (puzzle 22))

(def content ["on x=10..12,y=10..12,z=10..12"
              "on x=11..13,y=11..13,z=11..13"
              "off x=9..11,y=9..11,z=9..11"
              "on x=10..10,y=10..10,z=10..10"])

(def regexp #"(on|off) x=([0-9]*)..([0-9]*),y=([0-9]*)..([0-9]*),z=([0-9]*)..([0-9]*)")

(defn parse-line [line]
  (let [[state & values] (drop 1 (re-find regexp line))
        values (map parse-long values)]
    {:state state
     :vec (partition 2 values)}))

(defn count-on [lines]
  (->> lines
       (map (fn [line] (map (comp inc #(Math/abs (reduce - %))) (:vec line))))
       (map (partial reduce *))
       (reduce +)))

(defn remove-cube [lines {:keys [[x1 x2 y1 y2 z1 z2]]}])

(defn append-cube [lines {[[x1 x2] [y1 y2] [z1 z2]] :vec}]
  (mapcat (fn [line]
            (let [[[x1' x2'] [y1' y2'] [z1' z2']] (:vec line)]
              [line
               {:state "on" :vec [[x2 x2'] [y2 y2'] [z2 z2']]}]))
          lines))

(defn handle-cube [lines {:keys [state] :as cube}]
  (if (= state "off")
    lines
    #_
    (remove-cube lines cube)
    (append-cube lines cube)))

(->> content
     (map parse-line)
     (take 2)
     (#(reduce handle-cube [(first %)] (rest %)))
     #_
     (count-on))

(map parse-line content)

""
(->> content
     (map ))

content
