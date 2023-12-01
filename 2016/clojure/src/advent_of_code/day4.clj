(ns advent-of-code.day4
  (:require [clojure.string :as str])
  (:import java.security.MessageDigest))

(def input "bgvyzdsv")

(defn md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn solve [zeroes]
  (->> (range)
       (map (fn [i] [i (md5 (str input i))]))
       (filter (fn [[_ s]] (str/starts-with? s zeroes)))
       (first)
       first))

(def solution1 (solve "00000"))
(def solution2 (solve "000000"))
