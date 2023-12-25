(ns advent-of-code.day20
  (:require [advent-of-code.core :refer [puzzle]]
            [clojure.string :as str]))

(def content (puzzle 20))

(def content
  ["broadcaster -> a, b, c"
   "%a -> b"
   "%b -> c"
   "%c -> inv"
   "&inv -> a"])

(defn parse-line [line]
  (let [[_ source dest] (re-find #"(.*) -> (.*)" line)
        dests (str/split dest #", ")
        name (apply str (rest source))
        type (first source)]
    (case type
      \%
      [name {:dests dests :type :flipflop :state :off}]
      \&
      [name {:dests dests :type :conjunction :memory {}}]
      ["broadcaster" {:dests dests :type :broadcaster}])))

(def machine (into {} (map parse-line content)))

(defn update-memory [machine pulse src dest]
  (let [{:keys [type]} (get machine dest)]
    (if (= :conjunction type)
      (update-in machine [dest :memory] assoc src pulse)
      machine)))

(defn handle-module [machine pulse module-name]
  (let [{:keys [type memory state dests]} (get machine module-name)]
    (prn module-name type)
    (case type
      :broadcaster
      (reduce (fn [machine dest] (-> (handle-module machine pulse dest)
                                     (update-memory pulse module-name dest)))
              machine
              dests)
      :flipflop
      (if (= pulse :high)
        machine
        (reduce (fn [machine dest]
                  (let [pulse' (if (= :off state) :high :low)
                        state' (if (= :off state) :on :off)]
                    (-> machine
                        (update module-name assoc :state state')
                        (handle-module pulse' dest)
                        (update-memory pulse module-name dest))))
                machine
                dests))
      :conjunction
      (let [related (map (comp vals :dest #(get machine %)) dests)
            high? (and (seq related) (every? (partial = :high) related))
            pulse' (if high? :low :high)]
        (reduce
         (fn [machine dest]
           (-> (handle-module machine pulse' dest)
               (update-memory pulse' module-name dest)))
         machine
         dests)))))

(defn press-button []
  (loop [machine machine
         signal :low]
    (handle-module machine signal "broadcaster")))

(press-button)
