(ns day15.main
  (:require [clojure.math.combinatorics :refer [selections]]))

; solution taken from https://github.com/derekslager

(defn parse-input [resource]
  (into [] (for [[_ ingredient props] (re-seq #"(\w+): (.*)" resource)]
             [ingredient (into {} (for [[_ k v] (re-seq #"(\w+) (-?\d+)" props)]
                                    [(keyword k) (Integer/parseInt v)]))])))

(defn score-key [stats recipe key]
  (max 0 (reduce + (map * recipe (for [[_ stat] stats] (key stat))))))

(defn score [stats recipe]
  (apply *
         (for [key [:capacity :durability :flavor :texture]
               :let [score (score-key stats recipe key)]
               :while (not= 0 score)]
           score)))

(defn -main [& args]
  (let [resource (slurp (first args))
        stats (parse-input resource)
        ingredients (set (map first stats))]
    ;; part 1
    (println (apply max
           (for [recipe (selections (range 1 97) (count ingredients))
                 :when (= 100 (reduce + recipe))
                 ;; part 2
                 ;:when (= 500 (score-key stats recipe :calories))
                 ]
             (score stats recipe))))))