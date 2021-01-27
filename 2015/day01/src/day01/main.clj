(ns day01.main)


(def symbol-map { 
                 \( inc
                 \) dec 
                 })

(defn read-input [path] (slurp path))

(defn apply-symbol [acc idx symbol]
  (if (= acc -1) (println "Basement position: " idx))
  ((get symbol-map symbol identity) acc))

(defn reduce-symbols [symbols]
  (reduce-kv apply-symbol 0 (apply vector (map char symbols))))


(defn -main [& args]
  (let [path (first args)]
    (println "Reading " path "...")
    (println "Output is: " (reduce-symbols (read-input path)))))
