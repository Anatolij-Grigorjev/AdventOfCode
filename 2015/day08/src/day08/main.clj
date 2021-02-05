(ns day08.main
  (:require [aoc-commons.core :refer :all]))

(defn code-count [input-word] (count input-word))

(defn num-escapes-pattern [word re]
  (count (re-seq re word)))

(defn data-count [input-word]
  (let [code-size (code-count input-word)
        quotes-size 2
        count-patterns-in-contents (partial num-escapes-pattern input-word)
        num-escapes-dash (count-patterns-in-contents #"\\\\")
        num-escapes-quote (count-patterns-in-contents #"\\\"")
        num-escapes (+ num-escapes-dash num-escapes-quote)
        num-unicode-marks (count-patterns-in-contents #"\\x[0-9a-fA-F][0-9a-fA-F]")]
    (println input-word ": " code-size "-" quotes-size "-(" num-escapes-quote "+" num-escapes-dash ")-(3*" num-unicode-marks ")")
    (- code-size quotes-size num-escapes (* num-unicode-marks 3))))


(defn code-data-counts [input-word]
  (let [code-size (code-count input-word)
        data-size (data-count input-word)]
    (println input-word ": code " code-size " - data " data-size)
    [code-size data-size]))

(defn -main [& args]
  (let [path (first args)
        input (read-input-lines identity path)]
    (println "Read input from path " path)
    (println "Total of input lines: " (count input))
    (let [found-sizes (map code-data-counts input)
          code-size (reduce + (map first found-sizes))
          data-size (reduce + (map second found-sizes))]
      (println "Total code size: " code-size " total data size " data-size)
      (println "Total day01 diff: " (- code-size data-size)))))
