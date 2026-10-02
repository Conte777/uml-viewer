(ns uml-viewer.main.go-crap
  (:require [uml-viewer.application.ir-generator :as ir-generator]
            [uml-viewer.go-language.crap-go :as crap-go])
  (:gen-class))

(defn -main [& args]
  (let [policy-path (or (first args) "examples/uml-viewer.policy.edn")
        out (or (second args) ".metrics/crap.edn")
        trees (filter #(= :go (:lang %))
                      (ir-generator/source-trees (ir-generator/read-policy policy-path)))]
    (println "Wrote" (crap-go/write! trees out) "Go entries to" out)))
