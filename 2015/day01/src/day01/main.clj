(ns day01.main)


(def symbol-map { 
                 \( inc
                 \) dec 
                 })

(defn read-input [path] (slurp path))

(defn apply-symbol [acc symbol]
  ((get symbol-map symbol identity) acc))

(defn reduce-symbols [symbols]
  (reduce apply-symbol 0 symbols))


(defn -main [& args]
  (let [path (first args)]
    (println "Reading " path "...")
    (println "Output is: " (reduce-symbols (read-input path)))))
