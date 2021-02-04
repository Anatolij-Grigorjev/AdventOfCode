(ns day08.main
  (:require [clojure.java.io :as io]))


(defn read-input [path]
  (with-open [reader (io/reader path)]
    (doall (map println (line-seq reader)))))


(defn -main [& args]
  (let [path (first args)
        input (read-input path)]
    (println "Read input from path " path)
    (println "Total of input lines: " (count input))))
