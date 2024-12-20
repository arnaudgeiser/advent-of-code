(ns advent-of-code.core
  (:require [environ.core :as env]
            [clj-http.client :as client]
            [clojure.string :as str]))

(def url (partial format "https://adventofcode.com/2024/day/%d/input"))
(def session (env/env :aoc-session))
(def headers {"Cookie" (format "session=%s" session)})

(defn puzzle [day]
  (->> (client/get (url day) {:headers headers})
       :body
       (#(str/split % #"\n"))))

(defn raw-puzzle [day]
  (->> (client/get (url day) {:headers headers})
       :body))

(defn submit [day level answer]
  (client/post (format "https://adventofcode.com/2024/day/%d/answer" day)
               {:body (format "level=%s&answer=%s" level answer)}))

(defn neighbors [content [x y]]
  (->> [[x (dec y)]
        [x (inc y)]
        [(dec x) y]
        [(inc x) y]
        [(dec x) (dec y)]
        [(dec x) (inc y)]
        [(inc x) (dec y)]
        [(inc x) (inc y)]]
       (remove (fn [[x y]]
                 (or (neg? x)
                     (neg? y)
                     (>= x (count content))
                     (>= y (count (first content))))))))
