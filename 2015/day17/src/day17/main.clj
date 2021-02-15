(ns day17.main
  (:require [aoc-commons.core :as aoc]))

(defn parse-bucket [line]
  ((comp first first) (re-seq #"(\d+)" line)))

(defn -main [& args]
  (let [path (first args)
        buckets (aoc/read-input-lines parse-bucket path)]
    (println buckets)))
