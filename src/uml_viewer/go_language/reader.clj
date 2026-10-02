(ns uml-viewer.go-language.reader
  "Reading Go: packages from go list or the sources, and declarations in a file."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [uml-viewer.graph :as graph]))

(def ^:private list-template
  "{{.Dir}}\t{{.ImportPath}}\t{{.Name}}\t{{join .GoFiles \",\"}},{{join .CgoFiles \",\"}}\t{{join .Imports \",\"}}")

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
  (let [pb (doto (ProcessBuilder. ^java.util.List ["go" "list" "-e" "-f" list-template "./..."])
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

(defn- blank-inside [^String lit]
  (if (< (count lit) 2)
    lit
    (str (subs lit 0 1)
         (str/replace (subs lit 1 (dec (count lit))) #"[^\n]" " ")
         (subs lit (dec (count lit))))))

(defn mask-comments
  "Comments become spaces. String, raw string, and rune literals are kept,
  or with `blank?` keep only their quotes."
  [^String source blank?]
  (let [n (count source)
        literal (if blank? blank-inside identity)
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
              (.append sb (literal (subs source i end)))
              (recur end))

            (or (= c \") (= c \'))
            (let [end (loop [j (inc i)]
                        (cond
                          (>= j n) n
                          (= (.charAt source j) \\) (recur (+ j 2))
                          (= (.charAt source j) c) (inc j)
                          (= (.charAt source j) \newline) j
                          :else (recur (inc j))))]
              (.append sb (literal (subs source i end)))
              (recur end))

            :else
            (do (.append sb c)
                (recur (inc i)))))))))

(def ^:private import-spec
  #"(?:([A-Za-z_][A-Za-z0-9_]*|\.)\s+)?(?:\"([^\"]*)\"|`([^`]*)`)")

(defn- import-specs
  "`{:name :path}` of each import in the header of masked `text`; `:name` is the alias."
  [text]
  (let [decls (re-matcher #"(?m)^(?:func|type|var|const)\b" text)
        header (subs text 0 (if (.find decls) (.start decls) (count text)))
        blocks (map second (re-seq #"(?m)^import\s*\(([^)]*)\)" header))
        singles (map second (re-seq #"(?m)^import\s+([^(\s][^\n]*)" header))]
    (->> (concat blocks singles)
         (mapcat #(re-seq import-spec %))
         (map (fn [[_ alias quoted raw]] {:name alias :path (or quoted raw)})))))

(defn- read-file
  "Package name and imports of Go `source`."
  [source]
  (let [text (mask-comments source false)]
    {:package (second (re-find #"(?m)^package\s+([A-Za-z_][A-Za-z0-9_]*)" text))
     :imports (vec (distinct (map :path (import-specs text))))}))

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

(defn go-source?
  "A file go build reads: `.go`, not a test, not hidden by `_` or `.`."
  [^java.io.File f]
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
                       (fn [d] (filter #(and (.isDirectory %)
                                             (not (java.nio.file.Files/isSymbolicLink (.toPath %))))
                                       (.listFiles d)))
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

(defn- package-meta [pkg root prefix ns-prefix]
  (let [relative (str/replace (relative-dir (:dir pkg) root) "/" ".")
        ns-str (ns-join ns-prefix relative)]
    (assoc pkg
      :ns ns-str
      :id (graph/id-of ns-str prefix))))

(defn close-of
  "Index just after the bracket that closes the one at `i`."
  [^String text i]
  (let [n (count text)]
    (loop [j i depth 0]
      (if (>= j n)
        n
        (let [c (.charAt text j)]
          (cond
            (#{\( \[ \{} c) (recur (inc j) (inc depth))
            (#{\) \] \}} c) (if (= 1 depth) (inc j) (recur (inc j) (dec depth)))
            :else (recur (inc j) depth)))))))

(defn- line-end
  "End of a one-line type expression at `i`: its newline, or a group's `)`."
  [^String text i]
  (let [n (count text)]
    (loop [j i depth 0]
      (if (>= j n)
        n
        (let [c (.charAt text j)]
          (cond
            (and (= c \newline) (zero? depth)) j
            (#{\( \[ \{} c) (recur (inc j) (inc depth))
            (#{\) \] \}} c) (if (zero? depth) j (recur (inc j) (dec depth)))
            :else (recur (inc j) depth)))))))

(defn- skip-blank [^String text i]
  (let [n (count text)]
    (loop [j i]
      (if (and (< j n)
               (let [c (.charAt text j)]
                 (or (Character/isWhitespace c) (= c \;))))
        (recur (inc j))
        j))))

(defn- ahead
  "Up to 256 characters of `text` from `i`, enough for a name or a keyword."
  [^String text i]
  (subs text i (min (count text) (+ i 256))))

(defn- body-type
  "`[:struct body end]`, `[:interface body end]`, or `[:other nil end]` for the type at `i`."
  [^String text i]
  (if-let [[head kind] (re-find #"^(struct|interface)\s*\{" (ahead text i))]
    (let [brace (+ i (dec (count head)))
          end (close-of text brace)]
      [(keyword kind) (subs text (inc brace) (max (inc brace) (dec end))) end])
    [:other nil (line-end text i)]))

(defn- type-entry [^String text i]
  (when-let [type-name (re-find #"^[A-Za-z_][A-Za-z0-9_]*" (ahead text i))]
    (let [j (+ i (count type-name))
          j (if (and (< j (count text)) (= \[ (.charAt text j))) (close-of text j) j)
          j (+ j (count (re-find #"^[ \t]*=?[ \t]*" (ahead text j))))
          [kind body end] (body-type text j)]
      {:name type-name :pos i :kind kind :body body :end end})))

(defn- group-entries [^String text open]
  (let [end (dec (close-of text open))]
    (loop [k (skip-blank text (inc open)) entries []]
      (if-let [e (when (< k end) (type-entry text k))]
        (recur (skip-blank text (:end e)) (conj entries e))
        entries))))

(defn- type-decls [^String text]
  (let [m (re-matcher #"(?m)^type\b" text)]
    (loop [acc []]
      (if-not (.find m)
        acc
        (let [i (skip-blank text (.end m))]
          (recur (if (and (< i (count text)) (= \( (.charAt text i)))
                   (into acc (group-entries text i))
                   (if-let [e (type-entry text i)] (conj acc e) acc))))))))

(defn- matches-at [re text]
  (let [m (re-matcher re text)]
    (loop [acc []]
      (if (.find m)
        (recur (conj acc {:pos (.start m) :groups (mapv #(.group m (int %)) (range 1 (inc (.groupCount m))))}))
        acc))))

(defn read-decls
  "Imports with aliases, types, functions, and methods of Go `source`."
  [source]
  (let [text (mask-comments source true)]
    {:imports (import-specs (mask-comments source false))
     :types (type-decls text)
     :funcs (mapv (fn [{:keys [pos groups]}] {:name (first groups) :pos pos})
                  (matches-at #"(?m)^func\s+([A-Za-z_]\w*)" text))
     :methods (mapv (fn [{:keys [pos groups]}]
                      {:recv (first groups) :name (second groups) :pos pos})
                    (matches-at #"(?m)^func\s*\(\s*(?:[A-Za-z_]\w*\s+)?\*?\s*([A-Za-z_]\w*)(?:\[[^\]]*\])?\s*\)\s*([A-Za-z_]\w*)"
                                text))}))

(defn package-index
  "Packages under `root`, from go list or else the sources:
  `{:dir :import-path :package :files :imports :ns :id}`."
  [root prefix ns-prefix]
  (let [rootf (.getCanonicalFile (io/file root))]
    (mapv #(package-meta % rootf prefix ns-prefix) (packages rootf))))
