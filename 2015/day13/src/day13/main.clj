(ns day13.main
  (:require [aoc-commons.core :refer :all])
  (:require [clojure.string :as str])
  (:require [ubergraph.core :as ug]))

(defn word-sign [word]
  (case word
    "gain" 1
    "lose" -1))

(defn items-pairs-ring [col]
  (let [start-finish-pairs (partition 2 1 col)
        first-node ((comp first first) start-finish-pairs)
        last-node ((comp last last) start-finish-pairs)]
    (conj (apply vector start-finish-pairs) [last-node first-node])))

(defn parse-people-relations [line]
  (let [sentence-words (str/split line #"\s")
        person-sitting (first sentence-words)
        points-sign (word-sign (nth sentence-words 2))
        points-amount (* points-sign (parse-int (nth sentence-words 3)))
        next-to-person (apply str (butlast (last sentence-words)))]
    [person-sitting next-to-person points-amount]))

(defn segment-length [g node1 node2]
  (ug/weight g (first (ug/find-edges g node1 node2))))

(defn total-path-length [g nodes]
  (let [segment-lengths (map #(segment-length g (first %) (second %)) (items-pairs-ring nodes))]
    (reduce + segment-lengths)))

(defn all-paths [g]
  (let [node-lists (permutations (ug/nodes g))]
    (map #(vector % (total-path-length g %)) node-lists)))

(defn max-by [f col]
  (first (reverse (sort-by f col))))

(defn path->str [path-pair]
  (str "longest path is: " (first path-pair) " -> " (second path-pair)))

(defn calc-print-longest-path [g]
  (let [all-paths-map (all-paths g)
        longest-path (max-by second all-paths-map)]
    (println (path->str longest-path))
    longest-path))

(defn build-graph [path]
  (apply ug/multigraph (read-input-lines parse-people-relations path)))

(defn -main [& args]
  (let [path (first args)
        input-graph (build-graph path)]
    (ug/pprint input-graph)
    (let [long-paths (map calc-print-longest-path (repeatedly 150 (partial build-graph path)))
          longest-path (max-by second long-paths)]
      (println)
      (println "DONE. Truly," (path->str longest-path)))))
