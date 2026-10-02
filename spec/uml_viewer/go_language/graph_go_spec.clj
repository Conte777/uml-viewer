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

(defn- write-bank [dir]
  (spit-file dir "go.mod" "module example.com/bank\n\ngo 1.21\n")
  (spit-file dir "deps/deps.go"
             (str "package deps\n\n"
                  "import \"errors\"\n\n"
                  "var ErrMissing = errors.New(\"missing\")\n\n"
                  "type Domain string\n\n"
                  "type ReleaseFn func() error\n\n"
                  "type Reader interface {\n"
                  "\tGet(id int) (string, error)\n"
                  "\tList() []string\n"
                  "}\n\n"
                  "type Store interface {\n"
                  "\tReader\n"
                  "\tPut(id int, v string) error\n"
                  "}\n\n"
                  "type NotFound struct{}\n\n"
                  "func (NotFound) Error() string { return \"\" }\n\n"
                  "type Clock interface {\n"
                  "\tNow() int64\n"
                  "}\n"))
  (spit-file dir "model/model.go"
             (str "package model\n\n"
                  "type Base struct{ ID int }\n\n"
                  "type User[T any] struct {\n"
                  "\tBase\n"
                  "\tData T\n"
                  "}\n\n"
                  "func (u *User[T]) Name() string { return \"}\" }\n\n"
                  "func NewUser() {}\n\n"
                  "func lower() {}\n"))
  (spit-file dir "store/postgres/repo.go"
             (str "package postgres\n\n"
                  "import (\n"
                  "\td \"example.com/bank/deps\"\n"
                  "\t\"example.com/bank/model\"\n"
                  ")\n\n"
                  "type (\n"
                  "\tRepo struct {\n"
                  "\t\t*model.Base\n"
                  "\t\tdb string `json:\"db}\"`\n"
                  "\t}\n"
                  "\tcache map[string]struct{}\n"
                  ")\n\n"
                  "func New() *Repo { return &Repo{} }\n\n"
                  "func (r *Repo) Get(id int) (string, error) { return \"\", nil }\n"
                  "func (r *Repo) List() []string { return nil }\n"
                  "func (r Repo) Put(id int, v string) error { return nil }\n"
                  "func helper() {}\n\n"
                  "var _ d.Clock = nil\n"))
  (spit-file dir "memory/mem.go"
             (str "package memory\n\n"
                  "import \"example.com/bank/model\"\n\n"
                  "type Mem struct {\n"
                  "\tInner struct {\n"
                  "\t\tmodel.Base\n"
                  "\t}\n"
                  "}\n\n"
                  "func (m *Mem) Now() int64 { return 0 }\n"
                  "func (m *Mem) Get(id int) (string, error) { return \"\", nil }\n"
                  "func (m *Mem) List() []string { return nil }\n"))
  (spit-file dir "clock/clock.go"
             (str "package clock\n\n"
                  "type Sys struct{}\n\n"
                  "func (Sys) Now() int64 { return 0 }\n"))
  (spit-file dir "audit/audit.go"
             (str "package audit\n\n"
                  "import \"example.com/bank/deps\"\n\n"
                  "type Log interface {\n"
                  "\tdeps.Reader\n"
                  "\tAppend(s string)\n"
                  "}\n"))
  (spit-file dir "journal/journal.go"
             (str "package journal\n\n"
                  "type J struct{}\n\n"
                  "func (j *J) Append(s string) {}\n"
                  "func (j *J) Get(id int) (string, error) { return \"\", nil }\n"
                  "func (j *J) List() []string { return nil }\n"))
  dir)

(describe "go types"
  (it "links implementations, embedding, ops, and interface packages"
    (let [g (graph/scan (graph/lookup :go) (write-bank (temp-root)) {:prefix "bank"})
          by-id (into {} (map (juxt :id identity) (:classes g)))
          typed (set (keep (fn [{:keys [from to kind]}]
                             (when (not= :dependency kind) [from to kind]))
                           (:edges g)))]
      (should= :interface (:stereotype (by-id :deps)))
      (should= #{[:store.postgres :deps :implements]
                 [:store.postgres :model :inheritance]
                 [:memory :deps :implements]
                 [:journal :deps :implements]
                 [:journal :audit :implements]
                 [:audit :deps :inheritance]}
               typed)
      (should= :interface (:stereotype (by-id :audit)))
      (should-be-nil (:stereotype (by-id :model)))
      (should-be-nil (:stereotype (by-id :store.postgres)))
      (should= [{:name "Repo" :text "Repo"}
                {:name "cache" :text "cache" :private true}
                {:name "New" :text "New"}
                {:name "Repo.Get" :text "Repo.Get" :private true}
                {:name "Repo.List" :text "Repo.List" :private true}
                {:name "Repo.Put" :text "Repo.Put" :private true}
                {:name "helper" :text "helper" :private true}]
               (:ops (by-id :store.postgres)))
      (should= ["Base" "User" "User.Name" "NewUser" "lower"]
               (map :name (:ops (by-id :model))))
      (should= ["Domain" "ReleaseFn" "Reader" "Store" "NotFound" "NotFound.Error" "Clock"]
               (map :name (:ops (by-id :deps)))))))

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
          _ (doseq [root [listed parsed]]
              (spit-file root "internal/model/native.go"
                         (str "package model\n\n"
                              "// #include <stdlib.h>\n"
                              "import \"C\"\n\n"
                              "type Native struct{}\n")))
          _ (java.nio.file.Files/createSymbolicLink
              (.toPath (io/file parsed "internal/model/loop"))
              (.toPath (io/file parsed "internal"))
              (make-array java.nio.file.attribute.FileAttribute 0))
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
      (should= (count (:classes from-list)) (count (:classes from-source)))
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
