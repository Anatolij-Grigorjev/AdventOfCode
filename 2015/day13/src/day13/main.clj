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

(defn segment-length [g [node1 node2]]
  (let [
        edge (ug/find-edge g node1 node2)
        weight (ug/weight g edge)]
    weight))

(defn segments-lengths [g node-pairs]
  (map (partial segment-length g) node-pairs))

(defn invert-pairs [col]
  (map reverse (reverse col)))

(defn total-path-length [g nodes]
  (let [nodes-ring (items-pairs-ring nodes)
        segment-lengths-fwd (doall (segments-lengths g nodes-ring))
        segment-lengths-back (doall (segments-lengths g (invert-pairs nodes-ring)))]
    (reduce + (concat segment-lengths-fwd segment-lengths-back))))

(defn all-paths [g]
  (let [node-lists (permutations (ug/nodes g))]
    (map #(vector % (total-path-length g %)) node-lists)))

(defn path->str [path-pair]
  (let [nodes (first path-pair)
        ring-nodes (conj (apply vector nodes) (first nodes))]
    (str "longest 2-way path is: " ring-nodes " -> " (second path-pair))))

(defn calc-longest-path [g]
  (let [all-paths-map (all-paths g)
        longest-path (max-by second all-paths-map)]
    longest-path))

(defn calc-print-longest-path [g]
  (let [longest-path (calc-longest-path g)]
    (println (path->str longest-path))))

(defn build-graph [path]
  (apply ug/multidigraph (read-input-lines parse-people-relations path)))

(defn edges-weight-0 [nodes-from nodes-to]
  (map (fn [node1 node2] [node1 node2 0]) nodes-from nodes-to))

(defn -main [& args]
  (let [path (first args)
        input-graph (build-graph path)]
    (ug/pprint input-graph)
    (calc-print-longest-path input-graph)
    (let [node-name "Author"
          og-nodes (ug/nodes input-graph)
          repeated-new-node (repeat (count og-nodes) node-name)
          graph-with-author (ug/add-nodes input-graph node-name)
          graph-connected (ug/add-directed-edges* graph-with-author (concat (edges-weight-0 repeated-new-node og-nodes) (edges-weight-0 og-nodes repeated-new-node)))]
      (ug/pprint graph-connected)
      (calc-print-longest-path graph-connected))))
