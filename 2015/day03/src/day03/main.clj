(ns day03.main)

(defn read-input [path] (slurp path))

; solution taken from https://github.com/derekslager

(defn move-pos [[x y] direction]
  (case direction
    \^ [x (+ y 1)]
    \v [x (- y 1)]
    \> [(+ x 1) y]
    \< [(- x 1) y]
    [x y]))

(defn moves-between-houses [all-directions]
  (into #{} (reductions move-pos [0 0] all-directions)))


(defn do-part-one [input]
  (println "Part1 - houses visited: " (count (moves-between-houses input))))


(defn -main [& args]
  (let [path (first args)]
    (println "Reading " path "...")
    (let [input (read-input path)]
      (do-part-one input)
      )))
