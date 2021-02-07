(ns day09.main
  (:require [aoc-commons.core :refer :all])
  (:require [clojure.string :as str])
  (:require [ubergraph.core :as ug])
  (:require [ubergraph.alg :as galg]))


(defn parse-distances [line]
  (let [city-from-and-rest (str/split line #" to " 2)
        city-from (first city-from-and-rest)
        city-to-and-distance (str/split (second city-from-and-rest) #" = " 2)
        city-to (first city-to-and-distance)]
    [city-from city-to (parse-int (second city-to-and-distance))]))


(defn -main [& args]
  (let [path (first args)
        input-graph (apply ug/graph (read-input-lines parse-distances path))]
    (println (ug/pprint input-graph))
    (let [graph-paths (for [start-city (ug/nodes input-graph)
                            end-city (ug/nodes input-graph)
                            :when (not= start-city end-city)]
                        (galg/shortest-path input-graph start-city end-city :weight))
          full-paths (filter #(= (count (ug/nodes input-graph)) (count (galg/nodes-in-path %))) graph-paths)
          cost-paths (map (fn [path]
                            {:cost (galg/cost-of-path path)
                             :edges (count (galg/edges-in-path path))
                             :from (galg/start-of-path path)
                             :to (galg/end-of-path path)}) full-paths)]
      (doall (map println cost-paths)))))
