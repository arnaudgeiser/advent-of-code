(ns advent-of-code.core
  (:require [environ.core :as env]
            [clj-http.client :as client]
            [clojure.string :as str]))

(def url (partial format "https://adventofcode.com/2015/day/%d/input"))
(def session (env/env :aoc-session))
(def headers {"Cookie" (format "session=%s" session)})

(defn puzzle [day]
  (->> (client/get (url day) {:headers headers})
       :body
       (#(str/split % #"\n"))))
