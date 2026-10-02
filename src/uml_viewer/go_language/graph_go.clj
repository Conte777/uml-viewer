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

(defn- mask-comments
  "Comments become spaces. String, raw string, and rune literals are kept."
  [^String source]
  (let [n (count source)
        sb (StringBuilder. n)]
    (loop [i 0]
      (if (>= i n)
        (str sb)
        (let [c (.charAt source i)
              nxt (when (< (inc i) n) (.charAt source (inc i)))]
          (cond
            (and (= c \/) (= nxt \/))
            (let [j (or (str/index-of source \newline i) n)]
              (dotimes [_ (- j i)] (.append sb \space))
              (recur j))

            (and (= c \/) (= nxt \*))
            (let [end (min n (+ 2 (or (str/index-of source "*/" (+ i 2)) n)))]
              (doseq [k (range i end)]
                (.append sb (if (= \newline (.charAt source k)) \newline \space)))
              (recur end))

            (= c \`)
            (let [end (min n (inc (or (str/index-of source \` (inc i)) n)))]
              (.append sb (subs source i end))
              (recur end))

            (or (= c \") (= c \'))
            (let [end (loop [j (inc i)]
                        (cond
                          (>= j n) n
                          (= (.charAt source j) \\) (recur (+ j 2))
                          (= (.charAt source j) c) (inc j)
                          (= (.charAt source j) \newline) j
                          :else (recur (inc j))))]
              (.append sb (subs source i end))
              (recur end))

            :else
            (do (.append sb c)
                (recur (inc i)))))))))

(def ^:private import-spec
  #"(?:[A-Za-z_][A-Za-z0-9_]*\s+|[._]\s*)?(?:\"([^\"]*)\"|`([^`]*)`)")

(defn- spec-paths [text]
  (->> (re-seq import-spec text)
       (map (fn [[_ quoted raw]] (or quoted raw)))))

(defn- read-file
  "Package name and imports of Go `source`."
  [source]
  (let [text (mask-comments source)
        decls (re-matcher #"(?m)^(?:func|type|var|const)\b" text)
        header (subs text 0 (if (.find decls) (.start decls) (count text)))
        blocks (map second (re-seq #"(?m)^import\s*\(([^)]*)\)" header))
        singles (map second (re-seq #"(?m)^import\s+([^(\s][^\n]*)" header))]
    {:package (second (re-find #"(?m)^package\s+([A-Za-z_][A-Za-z0-9_]*)" text))
     :imports (vec (distinct (mapcat spec-paths (concat blocks singles))))}))

(defn- module-file [root]
  (loop [dir root]
    (when dir
      (let [f (io/file dir "go.mod")]
        (if (.isFile f) f (recur (.getParentFile dir)))))))

(defn- module-path [mod-file]
  (when mod-file
    (second (re-find #"(?m)^module\s+\"?([^\s\"]+)" (slurp mod-file)))))

(defn- skip-dir? [^java.io.File dir root]
  (let [name (.getName dir)]
    (and (not= dir root)
         (or (str/starts-with? name ".")
             (str/starts-with? name "_")
             (#{"vendor" "testdata"} name)
             (.isFile (io/file dir "go.mod"))))))

(defn- go-source? [^java.io.File f]
  (let [name (.getName f)]
    (and (.isFile f)
         (str/ends-with? name ".go")
         (not (str/ends-with? name "_test.go"))
         (not (str/starts-with? name "."))
         (not (str/starts-with? name "_")))))

(defn- import-path-of [mod-dir mod-path dir]
  (let [rel (str/replace (str (.relativize (.toPath mod-dir) (.toPath dir))) #"\\" "/")]
    (cond
      (str/blank? rel) mod-path
      (str/blank? mod-path) rel
      :else (str mod-path "/" rel))))

(defn- read-sources [root]
  (let [mod-file (module-file root)
        mod-dir (if mod-file (.getParentFile mod-file) root)
        mod-path (module-path mod-file)
        dirs (tree-seq (fn [d] (and (.isDirectory d) (not (skip-dir? d root))))
                       (fn [d] (filter #(.isDirectory %) (.listFiles d)))
                       root)]
    (->> dirs
         (remove #(skip-dir? % root))
         (keep (fn [dir]
                 (let [files (sort-by #(.getName %) (filter go-source? (.listFiles dir)))
                       read (map #(read-file (slurp %)) files)]
                   (when (seq files)
                     {:dir (.getCanonicalFile dir)
                      :import-path (import-path-of mod-dir mod-path dir)
                      :package (:package (first read))
                      :files (mapv #(.getName %) files)
                      :imports (vec (sort (distinct (mapcat :imports read))))}))))
         (sort-by :import-path)
         vec)))

(defn- packages [root]
  (try
    (run-go-list root)
    (catch Exception e
      (binding [*out* *err*]
        (println (str "go list unavailable, reading sources: " (ex-message e))))
      (read-sources root))))

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
          pkgs (mapv #(package-meta % rootf prefix ns-prefix) (packages rootf))
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
