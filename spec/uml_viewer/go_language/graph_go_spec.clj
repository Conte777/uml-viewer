(ns uml-viewer.go-language.graph-go-spec
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [speclj.core :refer :all]
            [uml-viewer.application.ir-generator :as ir-generator]
            [uml-viewer.go-language.graph-go]
            [uml-viewer.graph :as graph]))

(defn- spit-file [dir rel content]
  (let [f (io/file dir rel)]
    (io/make-parents f)
    (spit f content)
    f))

(defn- temp-root []
  (io/file (System/getProperty "java.io.tmpdir")
           (str "uml-go-" (System/nanoTime))))

(defn- write-shop [dir]
  (spit-file dir "go.mod"
             (str "module example.com/shop\n\ngo 1.21\n\n"
                  "require github.com/lib/pq v1.10.9\n"))
  (spit-file dir "shop.go"
             (str "package shop\n\n"
                  "import (\n"
                  "\t\"fmt\"\n\n"
                  "\t\"example.com/shop/internal/store\"\n"
                  ")\n\n"
                  "func Run() { fmt.Println(store.Open()) }\n"))
  (spit-file dir "internal/store/doc.go" "// Package store keeps orders.\npackage store\n")
  (spit-file dir "internal/store/store.go"
             (str "package store\n\n"
                  "import (\n"
                  "\t\"net/http\"\n"
                  "\t_ \"github.com/lib/pq\"\n"
                  "\tm \"example.com/shop/internal/model\"\n"
                  ")\n\n"
                  "func Open() *m.Order { _ = http.MethodGet; return nil }\n"))
  (spit-file dir "internal/store/store_test.go"
             (str "package store\n\n"
                  "import \"example.com/shop/internal/testkit\"\n\n"
                  "var _ = testkit.X\n"))
  (spit-file dir "internal/model/user.go" "package model\n\ntype User struct{}\n")
  (spit-file dir "internal/model/order.go" "package model\n\ntype Order struct{}\n")
  (spit-file dir "internal/testkit/kit_test.go" "package testkit\n\nvar X = 1\n")
  (spit-file dir "testdata/bad.go" "package bad\n\nimport \"example.com/shop/internal/model\"\n")
  (spit-file dir "_scratch/s.go" "package scratch\n\nimport \"example.com/shop/internal/model\"\n")
  (spit-file dir "cmd/app/main.go"
             (str "package main\n\n"
                  "import \"example.com/shop/internal/store\"\n\n"
                  "func main() { store.Open() }\n"))
  (spit-file dir "tools/go.mod" "module example.com/tools\n\ngo 1.21\n")
  (spit-file dir "tools/tool.go" "package tools\n\nimport \"example.com/shop/internal/model\"\n")
  dir)

(describe "go graph"
  (it "makes one class per package with import edges and foreign packages"
    (let [dir (write-shop (temp-root))
          g (graph/scan (graph/lookup :go) dir {:prefix "shop"})
          by-id (into {} (map (juxt :id identity) (:classes g)))
          project (set (remove #(get-in by-id [% :foreign]) (keys by-id)))
          edges (set (map (juxt :from :to :kind) (:edges g)))]
      (should= #{:shop :internal.store :internal.model :cmd.app} project)
      (should= "shop" (:ns (by-id :shop)))
      (should= "shop.internal.store" (:ns (by-id :internal.store)))
      (should= "Store" (:name (by-id :internal.store)))
      (should= :go (:lang (by-id :internal.store)))
      (should (str/ends-with? (:file (by-id :internal.store)) "internal/store/doc.go"))
      (should (str/ends-with? (:file (by-id :internal.model)) "internal/model/order.go"))
      (should (str/ends-with? (:file (by-id :shop)) "shop.go"))
      (should= #{[:shop :internal.store :dependency]
                 [:shop :fmt :dependency]
                 [:internal.store :internal.model :dependency]
                 [:internal.store :net.http :dependency]
                 [:internal.store :github.com.lib.pq :dependency]
                 [:cmd.app :internal.store :dependency]}
               edges)
      (should= {:id :net.http :name "net.http" :ns "net.http" :foreign true}
               (by-id :net.http))))

  (it "collapses foreign packages by the policy prefixes"
    (let [dir (write-shop (temp-root))
          doc (ir-generator/document
                (graph/lookup :go)
                {:title "Shop"
                 :prefix "shop"
                 :lang :go
                 :src (str dir)
                 :hierarchical true
                 :foreign [:net :github.com]})
          ids (set (map :id (:classes doc)))
          doc-edges (set (map (juxt :from :to :kind) (:edges doc)))]
      (should (contains? ids :net))
      (should (contains? ids :github.com))
      (should-not (contains? ids :fmt))
      (should (contains? doc-edges [:internal.store :github.com :dependency]))))

  (it "reads the sources itself when go list fails, with the same graph"
    (let [listed (write-shop (temp-root))
          parsed (write-shop (temp-root))
          _ (spit (io/file parsed "go.mod")
                  "module example.com/shop\n\ngo 1.99\n\nrequire github.com/lib/pq v1.10.9\n")
          _ (spit-file parsed "vendor/github.com/lib/pq/conn.go"
                       "package pq\n\nimport \"example.com/shop/internal/model\"\n")
          _ (spit-file parsed "internal/model/strings.go"
                       (str "/* import \"os\" */\n"
                            "// import \"os\"\n"
                            "package model\n\n"
                            "const site = \"http://example.com\" // import \"os\"\n"
                            "const raw = `\nimport \"os\"\n`\n"))
          _ (spit-file listed "internal/model/strings.go"
                       (str "package model\n\n"
                            "const site = \"http://example.com\"\n"))
          scan #(graph/scan (graph/lookup :go) % {:prefix "shop"})
          from-list (scan listed)
          err (java.io.StringWriter.)
          from-source (binding [*err* err] (scan parsed))]
      (should (str/includes? (str err) "go list"))
      (should= (set (:edges from-list)) (set (:edges from-source)))
      (should= (set (map #(dissoc % :file) (:classes from-list)))
               (set (map #(dissoc % :file) (:classes from-source))))
      (should= (set (map #(some-> (:file %) (str/replace (str (.getCanonicalPath listed)) ""))
                         (:classes from-list)))
               (set (map #(some-> (:file %) (str/replace (str (.getCanonicalPath parsed)) ""))
                         (:classes from-source))))))

  (it "names a source tree under its own namespace root"
    (let [dir (write-shop (temp-root))
          g (graph/scan (graph/lookup :go) (io/file dir "internal")
                        {:prefix "shop" :ns-prefix "shop.internal"})
          by-id (into {} (map (juxt :id identity) (:classes g)))]
      (should= "shop.internal.store" (:ns (by-id :internal.store)))
      (should (contains? (set (map (juxt :from :to) (:edges g)))
                         [:internal.store :internal.model])))))
