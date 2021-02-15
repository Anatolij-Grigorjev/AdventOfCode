(ns day18.main
  (:require [aoc-commons.core :as aoc]))

(defn parse-light [c]
  (case c
    \. 0
    \# 1))

(defn parse-lights-line [line]
  (apply vector (map parse-light line)))


(defn -main [& args]
  (let [path (first args)
        lights-grid (aoc/read-input-lines parse-lights-line path)]
    (println lights-grid)))
