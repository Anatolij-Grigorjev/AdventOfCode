(ns day16.main
  (:require [aoc-commons.core :refer :all]))

(def etalon-aunt {:children 3
                  :cats 7
                  :samoyeds 2
                  :pomeranians 3
                  :akitas 0
                  :vizslas 0
                  :goldfish 5
                  :trees 3
                  :cars 2
                  :perfumes 1})
(def aunt-prop-names (keys etalon-aunt))
(def empty-aunt (zipmap aunt-prop-names (repeat (count aunt-prop-names) nil)))

(defn parse-aunt-props [line-props-part]
  (into empty-aunt (for [[_ prop amount] (re-seq #"(\w+): (\d+)" line-props-part)]
                     [(keyword prop) (parse-int amount)])))

(defn parse-aunt [line]
  (for [[_ aunt-name props-line] (re-seq #"(\w+\s\w+): (.*)" line)]
    [aunt-name (parse-aunt-props props-line)]))

(defn -main [& args]
  (let [path (first args)
        aunts-partial-info (read-input-lines parse-aunt path)]))
