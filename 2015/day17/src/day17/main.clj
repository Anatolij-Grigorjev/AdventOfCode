(ns day17.main
  (:require [aoc-commons.core :as aoc])
  (:require [clojure.math.combinatorics :refer [subsets permutations]]))

(defn parse-bucket-capacity [line]
  ((comp aoc/parse-int first first) (re-seq #"(\d+)" line)))

(defn parse-buckets [path]
  (let [capacities (aoc/read-input-lines parse-bucket-capacity path)]
    (zipmap (map (partial str "bucket") (range (count capacities))) capacities)))


(defn -main [& args]
  (let [path (first args)
        buckets (apply vector (parse-buckets path))
        sum 150]
    (println (count buckets) "buckets ->" sum "liters")
    (let [fit-buckets (for [buckets-selection (subsets buckets)
                            :let [buckets-contain (reduce + 0 (map second buckets-selection))]
                            :when (= sum buckets-contain)]
                        buckets-selection)]
      (println "fit combinations:" (count fit-buckets))
      (let [fewest-buckets-size (count (apply min-key count fit-buckets))
            fewest-bucket-combos (filter #(= fewest-buckets-size (count %)) fit-buckets)]
        (println "smalles subsets count" (count fewest-bucket-combos))))))
