(ns day16.main
  (:require [aoc-commons.core :refer :all]))

(def etalon-aunt-props {:children 3
                  :cats 7
                  :samoyeds 2
                  :pomeranians 3
                  :akitas 0
                  :vizslas 0
                  :goldfish 5
                  :trees 3
                  :cars 2
                  :perfumes 1})
(def aunt-prop-names (keys etalon-aunt-props))
(def empty-aunt-props (zipmap aunt-prop-names (repeat (count aunt-prop-names) nil)))

(defn parse-aunt-props [line-props-part]
  (into empty-aunt-props (for [[_ prop amount] (re-seq #"(\w+): (\d+)" line-props-part)]
                     [(keyword prop) (parse-int amount)])))

(defn parse-aunt [line]
  (let [[_ aunt-name props-line] (re-seq #"(\w+\s\w+): (.*)" line)]
    [aunt-name (parse-aunt-props props-line)]))

(defn aunt-props [aunt]
  (second aunt))

(defn key-etalon-or-nil [aunt key]
  (let [value (key (aunt-props aunt))]
    (or (nil? value) (= value (key etalon-aunt-props)))))

(defn filter-aunts-key [all-aunts key]
  (filter #(key-etalon-or-nil % key) all-aunts))

(defn -main [& args]
  (let [path (first args)
        aunts-partial-info (read-input-lines parse-aunt path)]
    (println aunts-partial-info)
    (let [filtered-aunts (reduce filter-aunts-key aunts-partial-info aunt-prop-names)]
      (println "questionable aunts" (count filtered-aunts)))))
