(ns uml-viewer.go-language.source-go-spec
  (:require [clojure.java.io :as io]
            [speclj.core :refer :all]
            [uml-viewer.go-language.source-go]
            [uml-viewer.source :as source]))

(defn- package-dir []
  (let [dir (io/file (System/getProperty "java.io.tmpdir")
                     (str "uml-go-src-" (System/nanoTime)))]
    (doseq [[name text] {"doc.go" "// Package store keeps orders.\npackage store\n"
                         "store.go" (str "package store\n\n"
                                         "type Store struct{}\n\n"
                                         "func Open() *Store { return &Store{} }\n")
                         "orders.go" (str "package store\n\n"
                                          "func (s *Store) Orders() []int {\n"
                                          "\treturn nil\n}\n\n"
                                          "func (s Store) Close() {}\n\n"
                                          "type (\n"
                                          "\tCursor struct {\n"
                                          "\t\tPage int\n"
                                          "\t}\n"
                                          "\tPage int\n"
                                          ")\n")
                         "store_test.go" "package store\n\nfunc Helper() {}\n"}]
      (let [f (io/file dir name)]
        (io/make-parents f)
        (spit f text)))
    dir))

(describe "go extractor"
  (it "opens the package file named on the ident when no member is named"
    (let [dir (package-dir)
          doc (.getPath (io/file dir "doc.go"))
          found (source/member-source {:lang :go :ns "shop.store" :file doc})]
      (should= doc (:file found))
      (should-be-nil (:line found))))

  (it "finds a function or type in another file of the package"
    (let [dir (package-dir)
          ident {:lang :go :ns "shop.store" :file (.getPath (io/file dir "doc.go"))}
          open (source/member-source (assoc ident :name "Open"))
          store (source/member-source (assoc ident :name "Store"))]
      (should= (.getPath (io/file dir "store.go")) (:file open))
      (should= 5 (:line open))
      (should (re-find #"store\.go:5$" (:title open)))
      (should= 3 (:line store))))

  (it "finds a method by Type.Method"
    (let [dir (package-dir)
          ident {:lang :go :ns "shop.store" :file (.getPath (io/file dir "doc.go"))}
          orders (source/member-source (assoc ident :name "Store.Orders"))
          close (source/member-source (assoc ident :name "Store.Close"))]
      (should= (.getPath (io/file dir "orders.go")) (:file orders))
      (should= 3 (:line orders))
      (should= 7 (:line close))))

  (it "finds a type declared in a type group"
    (let [dir (package-dir)
          ident {:lang :go :ns "shop.store" :file (.getPath (io/file dir "doc.go"))}]
      (should= 10 (:line (source/member-source (assoc ident :name "Cursor"))))
      (should= 13 (:line (source/member-source (assoc ident :name "Page"))))))

  (it "ignores test files and missing members"
    (let [dir (package-dir)
          ident {:lang :go :ns "shop.store" :file (.getPath (io/file dir "doc.go"))}]
      (should-be-nil (source/member-source (assoc ident :name "Helper")))
      (should-be-nil (source/member-source (assoc ident :name "Missing"))))))
