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

(defn parse-aunt [line]
  )

(defn -main [& args]
  (let [path (first args)
        aunts-partial-info (read-input-lines parse-aunt path)]))
