(ns day08.main
  (:require [aoc-commons.core :refer :all]))


(defn -main [& args]
  (let [path (first args)
        input (read-input-lines println path)]
    (println "Read input from path " path)
    (println "Total of input lines: " (count input))))
