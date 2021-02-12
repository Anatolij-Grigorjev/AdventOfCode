(ns day14.main
  (:require [aoc-commons.core :refer :all]))


(defn parse-deer [line]
  (let [deer-name (first (re-seq #"\w+" line))
        line-numbers (map parse-int (re-seq #"\d+" line))]
    {:name deer-name
     :speed (first line-numbers)
     :fly (second line-numbers)
     :rest (nth line-numbers 2)}))

(defn travel-and-rest [deer]
  {:cycle-travel (* (:fly deer) (:speed deer)) 
   :cycle-seconds (+ (:fly deer) (:rest deer))})

(defn deer-after [seconds deer]
  (let [deer-travel (travel-and-rest deer)
        full-cycles (quot seconds (:cycle-seconds deer-travel))
        remainder-seconds (mod seconds (:cycle-seconds deer-travel))
        full-cycle-travel (* full-cycles (:cycle-travel deer-travel))
        remainder-travel (reduce + (take remainder-seconds (repeat (:fly deer) (:speed deer))))]
    (+ full-cycle-travel remainder-travel)))

(defn deer-path-str [[deer-name distance]]
  (str distance "->" deer-name))

(defn -main [& args]
  (let [path (first args)
        race-length 2503
        deers-list (read-input-lines parse-deer path)]
    (println "Competitors:" (map :name deers-list))
    (println "racing for" race-length "seconds...")
    (let [travel-distances (zipmap (map :name deers-list) (map (partial deer-after race-length) deers-list))
          best-deer (first (reverse (sort-by second travel-distances)))]
      (doall (map (comp println deer-path-str) travel-distances))
      (println "Best boy:" (deer-path-str best-deer)))))
