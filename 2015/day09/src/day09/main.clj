(ns day09.main
  (:require [aoc-commons.core :refer :all])
  (:require [clojure.string :as str])
  (:require [ubergraph.core :as ug]))


; all permutations of col, taken from https://stackoverflow.com/a/26076537
(defn permutations [colls]
  (if (= 1 (count colls))
    (list colls)
    (for [head colls
          tail (permutations (disj (set colls) head))]
      (cons head tail))))

(defn items-pairs [col]
  (loop [items col
         accum []]
    (let [l (first items)
          r (second items)]
      (if (nil? r)
        accum
        (recur (rest items) (conj accum [l r]))))))

(defn segment-length [g node1 node2]
  (ug/weight g (first (ug/find-edges g node1 node2))))

(defn total-path-length [g nodes]
  (let [segment-lengths (map #(segment-length g (first %) (second %)) (items-pairs nodes))]
    (reduce + segment-lengths)))

(defn all-paths [g]
  (let [node-lists (permutations (ug/nodes g))]
    (map #(vector % (total-path-length g %)) node-lists)))


(defn parse-distances [line]
  (let [city-from-and-rest (str/split line #" to " 2)
        city-from (first city-from-and-rest)
        city-to-and-distance (str/split (second city-from-and-rest) #" = " 2)
        city-to (first city-to-and-distance)]
    [city-from city-to (parse-int (second city-to-and-distance))]))


(defn node-in-subset [nodes n]
  (some #{n} nodes))

(defn edge->str [g edge]
  (str "-" (ug/weight g edge) "->" (ug/dest edge)))



(defn shortest-edges [g start]
  (loop [remaining-nodes (ug/nodes g)
         start-node start
         path-edges []]
    (let [node-edges (ug/find-edges g {:src start-node})
          forward-edges (filter #((partial node-in-subset remaining-nodes) (ug/dest %)) node-edges)
          shortest-edge (first (sort-by #(ug/weight g %) forward-edges))]
      (if (empty? forward-edges)
        path-edges
        (do
          (println "From: " start-node)
          (doall (map #(println "\t" (edge->str g %)) forward-edges))
          (println "Picked: " (edge->str g shortest-edge))
          (recur (remove #{start-node (ug/dest shortest-edge)} remaining-nodes)
                 (ug/dest shortest-edge)
                 (conj path-edges shortest-edge)))))))


(defn -main [& args]
  (let [path (first args)
        input-graph (apply ug/graph (read-input-lines parse-distances path))]
    (println (ug/pprint input-graph))
    (let [shortest-paths-map (map #(vector % (shortest-edges input-graph %)) (ug/nodes input-graph))]
      (doall (map (fn [[path-start shortest-path]]
                    (println "shortest path from: " path-start (map (partial edge->str input-graph) shortest-path))
                    (println "total shortest path: " (reduce + (map #(ug/weight input-graph %) shortest-path)))
                    (println))
                  shortest-paths-map)))
    (let [all-paths-map (all-paths input-graph)
          longest-path (first (reverse (sort-by second all-paths-map)))]
      (println "longest path is: " (first longest-path) " -> " (second longest-path)))))
