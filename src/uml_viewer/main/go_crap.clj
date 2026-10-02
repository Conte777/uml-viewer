(ns uml-viewer.main.go-crap
  (:require [uml-viewer.application.ir-generator :as ir-generator]
            [uml-viewer.go-language.crap-go :as crap-go])
  (:gen-class))

(defn -main [& args]
  (let [policy-path (or (first args) "examples/uml-viewer.policy.edn")
        out (or (second args) ".metrics/crap.edn")]
    (println "Wrote" (crap-go/write! (ir-generator/read-policy policy-path) out) "Go entries to" out)))
