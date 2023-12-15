(ns advent-of-code.day13
  (:require [advent-of-code.core :refer [raw-puzzle]]
            [clojure.string :as str]))

(def content (map str/split-lines (.split (raw-puzzle 13) "\\n\\n")))

(defn mirror= [a b]
  (= 1 (reduce + (mapcat (partial map #(if (= %1 %2) 0 1)) a b))))

(defn split-point [mountain eq-fn]
  (some (fn [i] (let [[top bottom] (split-at i mountain)
                      top (reverse top)
                      cnt (min (count top) (count bottom))
                      top (take cnt top)
                      bottom (take cnt bottom)]
                  (when (and (seq top) (seq bottom) (eq-fn top bottom)) i)))
        (range (count mountain))))

(defn revert-mountain [mountain] (apply map str mountain))

(defn split-mountain [mountain eq-fn]
  (or
   (split-point (revert-mountain mountain) eq-fn)
   (* (or (split-point mountain eq-fn) 0) 100)))

(defn solve [eq-fn] (transduce (map #(split-mountain % eq-fn)) + content))

(solve =) ;; 27664
(solve mirror=) ;; 33991
