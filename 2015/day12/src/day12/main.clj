(ns day12.main
  (:require [aoc-commons.core :refer :all])
  (:require [clojure.data.json :as json]))

(declare count-json-element)

(defn parsed-or-0 [num-str]
  (let [parsed (parse-int num-str)]
    (if (nil? parsed) 0 parsed)))

(defn sum-nums [col]
  (reduce + col))

(defn text-numers [text]
  (map parse-int (re-seq #"-?\d+" text)))

(defn sum-json-vector [json-array]
  (sum-nums (map count-json-element json-array)))

(defn sum-json-object [json-object]
  (let [fields-vals (vals json-object)]
    (if (some #(= "red" %) fields-vals)
      0
      (sum-nums (map count-json-element fields-vals)))))


(defn count-json-element [element]
  (cond
    (vector? element) (sum-json-vector element)
    (map? element) (sum-json-object element)
    (number? element) element
    :else (parsed-or-0 element)))





(defn -main [& args]
  (let [path (first args)
        json-string (slurp path)]
    (println "read at path" path)
    (println "JSON size:" (count json-string))
    (println "All numbers sum in JSON:" (sum-nums (text-numers json-string)))
    (println "All numbers sum without red objects:" (count-json-element (json/read-str json-string)))))
