(ns uml-viewer.go-language.crap-go-spec
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [speclj.core :refer :all]
            [uml-viewer.main.go-crap :as go-crap]))

(defn- spit-file [dir rel content]
  (let [f (io/file dir rel)]
    (io/make-parents f)
    (spit f content)
    f))

(defn- write-calc []
  (let [dir (io/file (System/getProperty "java.io.tmpdir") (str "uml-go-crap-" (System/nanoTime)))]
    (spit-file dir "go.mod" "module example.com/calc\n\ngo 1.21\n")
    (spit-file dir "calc.go"
               (str "package calc\n\n"
                    "type Acc struct{ n int }\n\n"
                    "func (a *Acc) Add(x int) int {\n"
                    "\tif x > 0 && x < 10 {\n"
                    "\t\ta.n += x\n"
                    "\t}\n"
                    "\treturn a.n\n"
                    "}\n\n"
                    "func Sign(x interface{}) int {\n"
                    "\tswitch {\n"
                    "\tcase x == 1:\n"
                    "\t\treturn 1\n"
                    "\tcase x == -1:\n"
                    "\t\treturn -1\n"
                    "\t}\n"
                    "\treturn 0\n"
                    "}\n\n"
                    "func Quote() string { return \"if { for && }\" }\n"))
    (spit-file dir "calc_test.go"
               (str "package calc\n\nimport \"testing\"\n\n"
                    "func TestSign(t *testing.T) { Sign(1); Sign(-1); Sign(0) }\n"))
    (spit-file dir "sub/sub.go" "package sub\n\nfunc F() {}\n")
    (spit-file dir "shapes/shapes.go" "package shapes\n\ntype Shape interface{ Area() int }\n")
    (spit-file dir "flaky/flaky.go" "package flaky\n\nfunc G() int { return 1 }\n")
    (spit-file dir "flaky/flaky_test.go"
               (str "package flaky\n\nimport \"testing\"\n\n"
                    "func TestG(t *testing.T) { if G() != 2 { t.Fatal(\"no\") } }\n"))
    (spit-file dir "calc.policy.edn" (pr-str {:prefix "calc" :lang :go :src (str dir)}))
    (spit-file dir ".metrics/crap.edn"
               (pr-str {:entries [{:name "keep" :namespace "other" :complexity 1 :coverage 100.0 :crap 1.0}
                                  {:name "Gone" :namespace "calc" :complexity 1 :coverage 0.0 :crap 2.0}
                                  {:name "Area" :namespace "calc.shapes" :complexity 1 :coverage 0.0 :crap 2.0}]}))
    dir))

(describe "go crap"
  (it "writes CRAP per function, keyed by package namespace, and keeps other namespaces"
    (let [dir (write-calc)
          out (io/file dir ".metrics/crap.edn")
          err (java.io.StringWriter.)]
      (binding [*err* err]
        (go-crap/-main (str (io/file dir "calc.policy.edn")) (str out)))
      (let [entries (:entries (edn/read-string (slurp out)))
            by-name (into {} (map (juxt (juxt :namespace :name) identity) entries))]
        (should= #{["other" "keep"] ["calc" "Acc.Add"] ["calc" "Sign"] ["calc" "Quote"]
                   ["calc.sub" "F"] ["calc.flaky" "G"]}
                 (set (keys by-name)))
        (should= {:complexity 3 :coverage 0.0 :crap 12.0}
                 (select-keys (by-name ["calc" "Acc.Add"]) [:complexity :coverage :crap]))
        (should= {:complexity 3 :coverage 100.0 :crap 3.0}
                 (select-keys (by-name ["calc" "Sign"]) [:complexity :coverage :crap]))
        (should= 1 (:complexity (by-name ["calc" "Quote"])))
        (should= 0.0 (:coverage (by-name ["calc.sub" "F"])))
        (should= 100.0 (:coverage (by-name ["calc.flaky" "G"])))
        (should= "Acc.Add" (:name (first entries)))
        (should (re-find #"FAIL" (str err)))))))
