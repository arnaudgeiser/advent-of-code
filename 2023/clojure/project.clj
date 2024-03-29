(defproject advent-of-code "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.0-alpha3"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [environ "1.2.0"]
                 [clj-http "3.12.3"]
                 [org.clojure/math.numeric-tower "0.0.5"]
                 [org.clojure/data.priority-map "1.1.0"]]
  :plugins [[lein-environ "1.2.0"]]
  :jvm-opts ["-Xss10m"]
  :repl-options {:init-ns advent-of-code.core})
