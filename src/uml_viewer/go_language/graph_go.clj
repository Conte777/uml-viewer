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

(defn- blank-inside [^String lit]
  (if (< (count lit) 2)
    lit
    (str (subs lit 0 1)
         (str/replace (subs lit 1 (dec (count lit))) #"[^\n]" " ")
         (subs lit (dec (count lit))))))

(defn- mask-comments
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

(defn- close-of
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

(defn- body-type
  "`[:struct body end]`, `[:interface body end]`, or `[:other nil end]` for the type at `i`."
  [^String text i]
  (if-let [[head kind] (re-find #"^(struct|interface)\s*\{" (subs text i))]
    (let [brace (+ i (dec (count head)))
          end (close-of text brace)]
      [(keyword kind) (subs text (inc brace) (max (inc brace) (dec end))) end])
    [:other nil (line-end text i)]))

(defn- type-entry [^String text i]
  (when-let [type-name (re-find #"^[A-Za-z_][A-Za-z0-9_]*" (subs text i))]
    (let [j (+ i (count type-name))
          j (if (and (< j (count text)) (= \[ (.charAt text j))) (close-of text j) j)
          j (+ j (count (re-find #"^[ \t]*=?[ \t]*" (subs text j))))
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

(defn- body-lines [body]
  (->> (str/split (str body) #"[\n;]")
       (map str/trim)
       (remove str/blank?)))

(defn- embeds-of [{:keys [kind body]}]
  (case kind
    :struct (keep #(second (re-matches #"\*?([A-Za-z_]\w*(?:\.[A-Za-z_]\w*)?)(?:\[[^\]]*\])?\s*(?:`\s*`|\"\s*\")?" %))
                  (body-lines body))
    :interface (keep #(re-matches #"[A-Za-z_]\w*(?:\.[A-Za-z_]\w*)?" %) (body-lines body))
    nil))

(defn- interface-methods [{:keys [body]}]
  (set (keep #(second (re-find #"^([A-Za-z_]\w*)\s*\(" %)) (body-lines body))))

(defn- matches-at [re text]
  (let [m (re-matcher re text)]
    (loop [acc []]
      (if (.find m)
        (recur (conj acc {:pos (.start m) :groups (mapv #(.group m (int %)) (range 1 (inc (.groupCount m))))}))
        acc))))

(defn- read-decls
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

(defn- exported? [s]
  (Character/isUpperCase (.charAt ^String s 0)))

(defn- file-bindings [imports by-path]
  (into {}
        (keep (fn [{:keys [name path]}]
                (when-let [target (by-path path)]
                  (let [bound (or name (:package target))]
                    (when-not (#{"_" "."} bound)
                      [bound (:id target)])))))
        imports))

(defn- resolve-ref [ref bindings own-id]
  (let [[qualifier type-name] (str/split ref #"\." 2)]
    (if type-name
      (when-let [id (bindings qualifier)] [id type-name])
      [own-id qualifier])))

(defn- op [op-name private?]
  (cond-> {:name op-name :text op-name}
    private? (assoc :private true)))

(defn- read-package [pkg by-path]
  (let [own (:id pkg)
        files (map-indexed (fn [idx f]
                             (let [d (read-decls (slurp (io/file (:dir pkg) f)))]
                               (assoc d :idx idx :bindings (file-bindings (:imports d) by-path))))
                           (sort (:files pkg)))
        refs (fn [file t] (keep #(resolve-ref % (:bindings file) own) (embeds-of t)))
        types (for [file files t (:types file)] (assoc t :idx (:idx file) :refs (refs file t)))
        funcs (for [file files f (:funcs file)] (assoc f :idx (:idx file)))
        methods (for [file files m (:methods file)] (assoc m :idx (:idx file)))
        exported-types (filter #(exported? (:name %)) types)
        ops (->> (concat (map #(assoc % :op (op (:name %) (not (exported? (:name %))))) types)
                         (map #(assoc % :op (op (:name %) (not (exported? (:name %))))) funcs)
                         (map #(assoc % :op (op (str (:recv %) "." (:name %)) true)) methods))
                 (sort-by (juxt :idx :pos))
                 (map :op)
                 (reduce (fn [acc o] (if (some #(= (:name o) (:name %)) acc) acc (conj acc o))) []))]
    {:ops ops
     :interfaces (into {}
                       (for [t types :when (= :interface (:kind t))]
                         [[own (:name t)] {:methods (interface-methods t) :embeds (:refs t)}]))
     :method-sets (reduce (fn [acc m] (update acc (:recv m) (fnil conj #{}) (:name m))) {} methods)
     :inherits (->> types (mapcat :refs) (map first) (remove #{own}) distinct vec)
     :stereotype (when (and (some #(= :interface (:kind %)) exported-types)
                            (not-any? #(= :struct (:kind %)) exported-types)
                            (not-any? #(exported? (:name %)) funcs))
                   :interface)}))

(defn- analyze [pkg by-path]
  (let [imports (remove #{"C"} (:imports pkg))
        ids (into {} (map (fn [[path target]] [path (:id target)])) by-path)]
    (merge
      {:id (:id pkg)
       :name (graph/module-name (:id pkg))
       :ns (:ns pkg)
       :lang :go
       :file (graph/relative-path (io/file (:dir pkg) (home-file pkg)))
       :requires (->> imports (keep ids) distinct vec)
       :foreigns (->> imports (remove ids) (map foreign-id) distinct vec)}
      (read-package pkg by-path))))

(defn- expand [interfaces k seen]
  (if (seen k)
    #{}
    (let [{:keys [methods embeds]} (interfaces k)]
      (into (set methods) (mapcat #(expand interfaces % (conj seen k)) embeds)))))

(defn- implementations
  "Package ids whose interfaces of two or more methods a type in `c` has every method of."
  [c contracts]
  (->> (for [[_ owned] (:method-sets c)
             [owner wanted] contracts
             :when (and (not= owner (:id c)) (every? owned wanted))]
         owner)
       distinct
       vec))

(defn- as-edges [c]
  (concat
    (map (fn [to] {:from (:id c) :to to :kind :implements}) (:impls c))
    (map (fn [to] {:from (:id c) :to to :kind :inheritance}) (:inherits c))
    (map (fn [to] {:from (:id c) :to to :kind :dependency}) (:requires c))
    (map (fn [to] {:from (:id c) :to to :kind :dependency}) (:foreigns c))))

(defn- foreign-class [id]
  {:id id :name (name id) :ns (name id) :foreign true})

(defn- public-class [c]
  (cond-> (dissoc c :requires :foreigns :interfaces :method-sets :inherits :impls)
    (empty? (:ops c)) (dissoc :ops)
    (nil? (:stereotype c)) (dissoc :stereotype)))

(defrecord GoGraph []
  graph/LanguageGraph
  (scan [_ root opts]
    (let [prefix (or (:prefix opts) "app")
          ns-prefix (or (:ns-prefix opts) prefix)
          rootf (.getCanonicalFile (io/file root))
          pkgs (mapv #(package-meta % rootf prefix ns-prefix) (packages rootf))
          by-path (into {} (map (juxt :import-path identity) pkgs))
          analyzed (mapv #(analyze % by-path) pkgs)
          interfaces (apply merge (map :interfaces analyzed))
          contracts (->> (keys interfaces)
                         (map (fn [k] [(first k) (expand interfaces k #{})]))
                         (filter #(>= (count (second %)) 2)))
          parsed (mapv #(assoc % :impls (implementations % contracts)) analyzed)
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
