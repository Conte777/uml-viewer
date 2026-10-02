(ns uml-viewer.go-language.graph-go
  "Go LanguageGraph: one class per package, import edges."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [uml-viewer.graph :as graph]))

(def ^:private list-template
  "{{.Dir}}\t{{.ImportPath}}\t{{.Name}}\t{{join .GoFiles \",\"}}\t{{join .Imports \",\"}}")

(defn- split-list [s]
  (vec (remove str/blank? (str/split (str s) #","))))

(defn- parse-line [line]
  (let [[dir import-path pkg-name files imports] (str/split line #"\t" -1)]
    {:dir (.getCanonicalFile (io/file dir))
     :import-path import-path
     :package pkg-name
     :files (split-list files)
     :imports (split-list imports)}))

(defn- run-go-list [root]
  (let [pb (doto (ProcessBuilder. ["go" "list" "-e" "-f" list-template "./..."])
             (.directory root))
        _ (.put (.environment pb) "GOPROXY" "off")
        proc (.start pb)
        err (future (slurp (.getErrorStream proc)))
        out (slurp (.getInputStream proc))
        code (.waitFor proc)]
    (when-not (zero? code)
      (throw (ex-info (str "go list failed: " (str/trim @err)) {:root (str root)})))
    (->> (str/split-lines out)
         (remove str/blank?)
         (map parse-line)
         (filter #(seq (:files %)))
         vec)))

(defn- relative-dir [dir root]
  (str/replace (str (.relativize (.toPath root) (.toPath dir))) #"\\" "/"))

(defn- ns-join [ns-prefix relative]
  (cond
    (str/blank? relative) (str ns-prefix)
    (str/blank? ns-prefix) relative
    :else (str ns-prefix "." relative)))

(defn- home-file
  "File the package opens at: doc.go, else <package>.go, else the first file."
  [{:keys [files package]}]
  (let [names (set files)]
    (or (names "doc.go")
        (names (str package ".go"))
        (first (sort files)))))

(defn- package-meta [pkg root prefix ns-prefix]
  (let [relative (str/replace (relative-dir (:dir pkg) root) "/" ".")
        ns-str (ns-join ns-prefix relative)]
    (assoc pkg
      :ns ns-str
      :id (graph/id-of ns-str prefix))))

(defn- foreign-id [import-path]
  (keyword (str/replace import-path "/" ".")))

(defn- analyze [pkg by-path]
  (let [imports (remove #{"C"} (:imports pkg))]
    {:id (:id pkg)
     :name (graph/module-name (:id pkg))
     :ns (:ns pkg)
     :lang :go
     :file (graph/relative-path (io/file (:dir pkg) (home-file pkg)))
     :requires (->> imports (keep by-path) distinct vec)
     :foreigns (->> imports (remove by-path) (map foreign-id) distinct vec)}))

(defn- as-edges [c]
  (concat
    (map (fn [to] {:from (:id c) :to to :kind :dependency}) (:requires c))
    (map (fn [to] {:from (:id c) :to to :kind :dependency}) (:foreigns c))))

(defn- foreign-class [id]
  {:id id :name (name id) :ns (name id) :foreign true})

(defn- public-class [c]
  (dissoc c :requires :foreigns))

(defrecord GoGraph []
  graph/LanguageGraph
  (scan [_ root opts]
    (let [prefix (or (:prefix opts) "app")
          ns-prefix (or (:ns-prefix opts) prefix)
          rootf (.getCanonicalFile (io/file root))
          pkgs (mapv #(package-meta % rootf prefix ns-prefix) (run-go-list rootf))
          by-path (into {} (map (juxt :import-path :id) pkgs))
          parsed (mapv #(analyze % by-path) pkgs)
          project-ids (set (map :id parsed))
          foreigns (->> parsed
                        (mapcat :foreigns)
                        distinct
                        (remove project-ids)
                        (mapv foreign-class))]
      {:classes (into (mapv public-class parsed) foreigns)
       :edges (->> (mapcat as-edges parsed)
                   (remove #(= (:from %) (:to %)))
                   distinct
                   vec)})))

(def impl (->GoGraph))

(graph/register! :go impl)
