(ns day12.main
  (:require [aoc-commons.core :refer :all])
  (:require [clojure.data.json :as json]))

(defn sum [col]
  (reduce + col))

(defn text-numers [text]
  (map parse-int (re-seq #"-?\d+" text)))







(defn -main [& args]
  (let [path (first args)
        json-string (slurp path)]
    (println "read at path" path)
    (println "JSON size:" (count json-string))
    (println "All numbers sum in JSON:" (sum (text-numers json-string)))))
