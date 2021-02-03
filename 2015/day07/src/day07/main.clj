(ns day07.main
  (:require [clojure.java.io :as io]))

(defn parse-logic-command [line] {})

(defn read-input [path]
  (with-open [reader (io/reader path)]
    (doall (map parse-logic-command (line-seq reader)))))


(defn -main [& args]
  (let [path (first args)
        commands (read-input path)]
    (println "Read commands from: " path)
    (println "Found total commands: " (count commands))
    (println "Day01 final signal wire a: ")))
