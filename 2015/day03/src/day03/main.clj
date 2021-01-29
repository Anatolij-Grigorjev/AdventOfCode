(ns day03.main)

(defn read-input [path] (slurp path))

; solution based on https://github.com/derekslager

(defn move-pos [[x y] direction]
  (case direction
    \^ [x (+ y 1)]
    \v [x (- y 1)]
    \> [(+ x 1) y]
    \< [(- x 1) y]
    [x y]))

(defn moves-between-houses [all-directions]
  (into #{} (reductions move-pos [0 0] all-directions)))

(defn odds [seq]
  (take-nth 2 seq))

(defn evens [seq]
  (take-nth 2 (rest seq)))


(defn do-part-one [input]
  (println "Part1 - houses visited: " (count (moves-between-houses input))))

(defn do-part-two [input]
  (println "Part2 - houses by santa + robosanta: " (count (into #{} (concat (moves-between-houses (odds input)) (moves-between-houses (evens input)))))))


(defn -main [& args]
  (let [path (first args)]
    (println "Reading " path "...")
    (let [input (read-input path)]
      (do-part-one input)
      (do-part-two input)
      )))
