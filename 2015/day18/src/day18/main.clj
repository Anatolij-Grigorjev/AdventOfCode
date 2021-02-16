(ns day18.main
  (:require [aoc-commons.core :as aoc]))

(defn parse-light [c]
  (case c
    \. 0
    \# 1))

(defn parse-lights-line [line]
  (apply vector (map parse-light line)))

(defn corners [grid]
  #{[0 0] [0 (dec (count grid))] [(dec (count grid)) 0] [(dec (count grid)) (dec (count grid))]})

;; part 2 - corner lights stuck on
(defn enable-corners [grid]
  (reduce (fn [grid [x y]]
            (assoc-in grid [x y] 1)) grid (corners grid)))

(defn neighbours-indices [[x y]]
  (disj (into #{} (for [around-x (range (dec x) (inc (inc x)))
                        around-y (range (dec y) (inc (inc y)))]
                    [around-x around-y])) [x y]))

(defn light-at [grid [x y]]
  (get-in grid [x y] 0))


(defn neighbours [grid [x y]]
  (let [indices (neighbours-indices [x y])]
    (map (partial light-at grid) indices)))


(defn apply-off-rules [grid [x y]]
  (let [surround-on (reduce + (neighbours grid [x y]))]
    (if (= 3 surround-on)
      1
      0)))

(defn apply-on-rules [grid [x y]]
  (let [surround-on (reduce + (neighbours grid [x y]))]
    (if (<= 2 surround-on 3)
      1
      0)))

(defn toggle-light [grid [x y]]
  (let [light-value (light-at grid [x y])]
    (if (zero? light-value)
      (apply-off-rules grid [x y])
      (apply-on-rules grid [x y]))))

(defn do-step [start-grid _]
  (let [row-size ((comp count first) start-grid)
        col-size (count start-grid)
        grid-corners (corners start-grid)
        lights-indices (for [x (range row-size)
                             y (range col-size)
                             ;; part 2 - dont process corners
                             :when (not (contains? grid-corners [x y]))]
                         [x y])]
    (loop [build-grid start-grid
           indices lights-indices]
      (let [next-idx (first indices)]
        (if (nil? next-idx)
          build-grid
          (recur
           (assoc-in build-grid next-idx (toggle-light start-grid next-idx))
           (rest indices)))))))


(defn -main [& args]
  (let [path (first args)
        lights-grid (enable-corners (aoc/read-input-lines parse-lights-line path))
        num-steps 100]
    (let [animated-grid (reduce do-step lights-grid (range num-steps))]
      (println "lights on" (reduce + (flatten animated-grid))))))
