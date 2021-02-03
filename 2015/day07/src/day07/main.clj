(ns day07.main
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(def OPS {:NOT bit-not
          :OR bit-or
          :AND bit-and
          :LSHIFT bit-shift-left
          :RSHIFT bit-shift-right
          :ASSIGN identity})

(defn parse-int [num-string]
  (try
    (Integer/parseInt num-string)
    (catch Exception e nil)))

(defn num-or-string [input]
  (let [num (parse-int input)]
    (if (nil? num) input num)))

(defn find-first [f col]
  (first (drop-while (complement f) col)))

(defn parse-op [line]
  (let [found-op-name (find-first #(.contains line (name %)) (keys OPS))]
    (if (nil? found-op-name)
      :ASSIGN
      found-op-name)))

(defn split-arrow [line]
  (str/split line #" -> " 2))

(defn parse-2-operands [line op]
  (let [operands-line (first (split-arrow line))
        operands (str/split operands-line (re-pattern (str " " (name op) " ")) 2)]
    (apply vector (map num-or-string operands))))

(defn parse-operands [line op]
  (case op
    :ASSIGN (vector (num-or-string (first (split-arrow line))))
    :NOT (vector (.substring (first (split-arrow line)) (count "NOT ")))
    (:OR :AND :LSHIFT :RSHIFT) (parse-2-operands line op)))

(defn parse-target [line]
  (second (split-arrow line)))

(defn parse-logic-command [line]
  (let [op (parse-op line)
        operands (parse-operands line op)
        target (parse-target line)]
    {:op (get OPS op)
     :operands operands
     :target target}))


(defn read-input [path]
  (with-open [reader (io/reader path)]
    (doall (map parse-logic-command (line-seq reader)))))

(defn map-wiring [wiring]
  { (:target wiring) { 
                      :f (fn [operands] 
                           (apply (:op wiring) operands)) 
                      :operands (:operands wiring)
                      }
   })

(defn func-grid [wirings]
  (into {} (map map-wiring wirings)))


(defn flush-wire [grid target]
  (let [wire (get grid target)
        wire-func (:f wire)
        wire-operands (:operands wire)]
    ;(println "Flusing wire |" target "| with operands: " wire-operands)
    (wire-func (map (fn [operand]
                     (if (number? operand)
                       operand
                       (flush-wire grid operand))) wire-operands))))


(defn -main [& args]
  (let [path (first args)
        commands (read-input path)]
    (println "Read commands from: " path)
    (println "Found total commands: " (count commands))
    (let [grid (func-grid commands)
          signal-a (flush-wire grid "a")]
      (println "Day01 final signal wire a: " signal-a))))
