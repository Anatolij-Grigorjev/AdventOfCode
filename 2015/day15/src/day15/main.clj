(ns day15.main
  (:require [clojure.math.combinatorics :refer [selections]]))

; solution based on https://github.com/derekslager

(defn parse-input [resource]
  (into [] (for [[_ ingredient props] (re-seq #"(\w+): (.*)" resource)]
             [ingredient (into {} (for [[_ k v] (re-seq #"(\w+) (-?\d+)" props)]
                                    [(keyword k) (Integer/parseInt v)]))])))

(defn score-key [stats recipe key]
  (max 0 (reduce + (map * recipe (for [[_ stat] stats] (key stat))))))

(defn score [stats recipe]
  (apply *
         (for [key [:capacity :durability :flavor :texture]
               :let [score (score-key stats recipe key)]]
           ;; removed :while not 0 shortcut 
           ;; it creates false result when only :texture score is 0
           score)))

(defn -main [& args]
  (let [resource (slurp (first args))
        stats (parse-input resource)
        ingredients (set (map first stats))]
    ;; part 1
    (doall (map println stats))
    (println)
    (let [best-cookie (apply max-key second
           (for [recipe (selections (range 1 97) (count ingredients))
                 :when (= 100 (reduce + recipe))
                 ;; part 2
                 :when (= 500 (score-key stats recipe :calories))]
             [recipe (score stats recipe)]))]
      (println "Best cookie balance:" (zipmap (map first stats) (first best-cookie)) "score:" (second best-cookie)))))