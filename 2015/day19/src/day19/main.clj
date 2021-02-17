(ns day19.main
  (:require [aoc-commons.core :as aoc])
  (:require [clojure.string :as str]))


(defn parse-inputs [data-string]
  (let [all-lines (str/split data-string #"\n")
        input-molecule (last all-lines)
        replacements (for [replacement all-lines
                           :while (not (str/blank? replacement))
                           :let [[[_ from to]] (re-seq #"(\w+) => (\w+)" replacement)]]
                       [from to])]
    {:replacements replacements
     :input-molecule input-molecule}))


(defn -main [& args]
  (let [path (first args)
        inputs (parse-inputs (slurp path))]
    (println inputs)))
