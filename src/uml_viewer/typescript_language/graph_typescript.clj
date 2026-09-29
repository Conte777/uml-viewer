(ns uml-viewer.typescript-language.graph-typescript
  "TypeScript LanguageGraph: one class per module, import edges, invoke names."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [uml-viewer.graph :as graph])
  (:import [java.util.regex Pattern]))

(def ^:private exts [".ts" ".tsx" ".js" ".jsx" ".mjs" ".cjs"])

(defn- excluded-name? [name]
  (or (str/ends-with? name ".d.ts")
      (re-find #"\.(?:test|spec)\.[cm]?[jt]sx?$" name)))

(defn- source-files [root]
  (let [root (.getCanonicalFile (io/file root))]
    (->> (file-seq root)
         (remove (fn [f]
                   (re-find #"/node_modules/|/dist/|/target/"
                            (str/replace (str f) #"\\" "/"))))
         (filter #(.isFile %))
         (filter #(re-find #"\.[cm]?[jt]sx?$" (.getName %)))
         (remove #(excluded-name? (.getName %)))
         (sort-by #(.getPath %)))))

(defn- regex-before?
  "True when `/` at `i` can open a regex literal rather than division."
  [source i]
  (loop [j (dec i)]
    (if (and (>= j 0) (Character/isWhitespace (.charAt ^String source j)))
      (recur (dec j))
      (or (neg? j)
          (let [c (.charAt ^String source j)]
            (cond
              (or (Character/isLetterOrDigit c) (#{\_ \$ \) \]} c))
              (let [start (loop [k j]
                            (if (and (>= k 0)
                                     (let [d (.charAt ^String source k)]
                                       (or (Character/isLetterOrDigit d)
                                           (#{\_ \$} d))))
                              (recur (dec k))
                              (inc k)))]
                (#{"return" "throw" "case" "void" "typeof" "delete"
                   "await" "yield" "else" "do" "in" "of"}
                 (subs source start (inc j))))
              :else true))))))

(defn- regex-end
  "Index just after the flags of a regex that opens at `i`, or nil."
  [source i]
  (let [n (count source)]
    (loop [j (inc i) class? false esc false]
      (when (< j n)
        (let [c (.charAt ^String source j)]
          (cond
            esc (recur (inc j) class? false)
            (= c \\) (recur (inc j) class? true)
            (= c \newline) nil
            (and (= c \[) (not class?)) (recur (inc j) true false)
            (and (= c \]) class?) (recur (inc j) false false)
            (and (= c \/) (not class?))
            (loop [k (inc j)]
              (if (and (< k n) (Character/isLetter (.charAt ^String source k)))
                (recur (inc k))
                k))
            :else (recur (inc j) class? false)))))))

(defn- mask-comments
  "Comments become spaces. String and template literals are copied and
  recorded as half-open ranges so a match inside them can be ignored.
  A regex literal is a hole too, so a quote inside `/\"/g` is not a string."
  [source]
  (let [n (count source)
        sb (StringBuilder. n)]
    (loop [i 0 holes []]
      (if (>= i n)
        {:text (str sb) :holes holes}
        (let [c (.charAt source i)
              nxt (when (< (inc i) n) (.charAt source (inc i)))]
          (cond
            (and (= c \/) (= nxt \/))
            (let [j (or (str/index-of source \newline i) n)]
              (dotimes [_ (- j i)] (.append sb \space))
              (recur j holes))

            (and (= c \/) (= nxt \*))
            (let [j (or (str/index-of source "*/" (+ i 2)) (- n 2))
                  end (min n (+ j 2))]
              (doseq [k (range i end)]
                (.append sb (if (= \newline (.charAt source k)) \newline \space)))
              (recur end holes))

            (and (= c \/) nxt (not= nxt \*) (regex-before? source i))
            (if-let [end (regex-end source i)]
              (do (.append sb (subs source i end))
                  (recur end (conj holes [i end])))
              (do (.append sb c)
                  (recur (inc i) holes)))

            (#{\' \" \`} c)
            (let [end (loop [j (inc i) esc false]
                        (cond
                          (>= j n) n
                          esc (recur (inc j) false)
                          (= (.charAt source j) \\) (recur (inc j) true)
                          (= (.charAt source j) c) (inc j)
                          :else (recur (inc j) false)))]
              (.append sb (subs source i end))
              (recur end (conj holes [i end])))

            :else
            (do (.append sb c)
                (recur (inc i) holes))))))))

(defn- in-hole? [holes i]
  (boolean (some (fn [[a b]] (and (<= a i) (< i b))) holes)))

(defn- header-of
  "Text of a declaration up to its body, assignment, or semicolon."
  [s]
  (loop [i 0 paren 0 bracket 0 brace 0]
    (if (>= i (count s))
      s
      (let [c (.charAt s i)]
        (cond
          (= c \() (recur (inc i) (inc paren) bracket brace)
          (= c \)) (recur (inc i) (max 0 (dec paren)) bracket brace)
          (= c \[) (recur (inc i) paren (inc bracket) brace)
          (= c \]) (recur (inc i) paren (max 0 (dec bracket)) brace)
          (= c \{) (if (and (zero? paren) (zero? bracket) (zero? brace))
                     (subs s 0 i)
                     (recur (inc i) paren bracket (inc brace)))
          (and (#{\= \;} c) (zero? paren) (zero? bracket) (zero? brace))
          (subs s 0 i)
          :else (recur (inc i) paren bracket brace))))))

(defn- match-paren [s open]
  (loop [i (inc open) depth 1]
    (when (< i (count s))
      (case (.charAt s i)
        \( (recur (inc i) (inc depth))
        \) (if (= depth 1) i (recur (inc i) (dec depth)))
        (recur (inc i) depth)))))

(defn- return-type [header]
  (when-let [open (str/index-of header "(")]
    (when-let [close (match-paren header open)]
      (second (re-find #"^\s*:\s*([A-Za-z_$][\w$]*)\s*$"
                       (subs header (inc close)))))))

(defn- const-type [header]
  (second (re-find #"\b(?:const|let|var)\s+[A-Za-z_$][\w$]*\s*:\s*([A-Za-z_$][\w$]*)\s*$"
                   header)))

(defn- statement-end [s start]
  (loop [i start paren 0 brace 0 bracket 0]
    (if (>= i (count s))
      (count s)
      (let [c (.charAt s i)]
        (cond
          (= c \() (recur (inc i) (inc paren) brace bracket)
          (= c \)) (recur (inc i) (max 0 (dec paren)) brace bracket)
          (= c \{) (recur (inc i) paren (inc brace) bracket)
          (= c \}) (recur (inc i) paren (max 0 (dec brace)) bracket)
          (= c \[) (recur (inc i) paren brace (inc bracket))
          (= c \]) (recur (inc i) paren brace (max 0 (dec bracket)))
          (and (= c \;) (zero? paren) (zero? brace) (zero? bracket)) (inc i)
          :else (recur (inc i) paren brace bracket))))))

(defn- from-spec [stmt]
  (second (re-find #"from\s*[\"']([^\"']+)[\"']" stmt)))

(defn- side-spec [stmt]
  (second (re-find #"^import\s*[\"']([^\"']+)[\"']" stmt)))

(defn- brace-body [stmt]
  (second (re-find #"\{([^}]*)\}" stmt)))

(defn- brace-names [stmt]
  (when-let [body (brace-body stmt)]
    (->> (str/split body #",")
         (map str/trim)
         (remove str/blank?)
         (keep (fn [part]
                 (let [part (str/replace part #"^type\s+" "")]
                   (or (second (re-find #"\bas\s+([A-Za-z_$][\w$]*)\s*$" part))
                       (second (re-find #"^([A-Za-z_$][\w$]*)" part))))))
         vec)))

(defn- default-binding [stmt]
  (second (re-find #"^import\s+(?:type\s+)?(?!type\b)([A-Za-z_$][\w$]*)\s*(?:,|\s+from\b)"
                   stmt)))

(defn- star-binding [stmt]
  (second (re-find #"\*\s+as\s+([A-Za-z_$][\w$]*)" stmt)))

(defn- word-at? [text i word]
  (and (.startsWith ^String text ^String word (int i))
       (let [before (when (pos? i) (.charAt text (dec i)))
             after-i (+ i (count word))
             after (when (< after-i (count text)) (.charAt text after-i))
             ident? #(and % (or (Character/isLetterOrDigit ^char %) (#{\_ \$} %)))]
         (and (not (ident? before)) (not (ident? after))))))

(defn- invoke-name [text i]
  (loop [j i depth-angle 0 depth-paren 0 started false]
    (when (< j (count text))
      (let [c (.charAt text j)]
        (cond
          (and (not started) (Character/isWhitespace c)) (recur (inc j) 0 0 false)
          (and (not started) (= c \<)) (recur (inc j) 1 0 true)
          (pos? depth-angle)
          (recur (inc j)
                 (cond (= c \<) (inc depth-angle)
                       (= c \>) (dec depth-angle)
                       :else depth-angle)
                 0
                 true)
          (and (not started) (= c \()) (recur (inc j) 0 1 true)
          (and started (zero? depth-angle) (zero? depth-paren) (= c \())
          (recur (inc j) 0 1 true)
          (pos? depth-paren)
          (cond
            (#{\' \"} c)
            (let [end (loop [k (inc j) esc false]
                        (cond
                          (>= k (count text)) (count text)
                          esc (recur (inc k) false)
                          (= (.charAt text k) \\) (recur (inc k) true)
                          (= (.charAt text k) c) k
                          :else (recur (inc k) false)))]
              (when (> end j)
                (subs text (inc j) end)))

            (Character/isWhitespace c) (recur (inc j) 0 depth-paren true)
            :else nil)
          :else nil)))))

(defn- invokes-of [text holes]
  (loop [i 0 acc []]
    (if-let [j (str/index-of text "invoke" i)]
      (if (and (word-at? text j "invoke") (not (in-hole? holes j)))
        (let [name (invoke-name text (+ j (count "invoke")))]
          (recur (inc j) (if name (conj acc name) acc)))
        (recur (inc j) acc))
      acc)))

(defn- line-starts [text holes keyword]
  (let [p (Pattern/compile (str "(?m)^\\s*" keyword "\\b"))
        m (.matcher p text)]
    (loop [acc []]
      (if (.find m)
        (let [i (.start m)]
          (recur (if (in-hole? holes i) acc (conj acc i))))
        acc))))

(defn- at-keyword [text at keyword]
  (or (str/index-of text keyword at) at))

(defn- export-fact [text at]
  (let [at (at-keyword text at "export")
        stmt (str/triml (subs text at (statement-end text at)))
        header (header-of (subs text at))]
    (cond
      (re-find #"^export\s+type\s*\{" stmt)
      {:spec (from-spec stmt)}

      (re-find #"^export\s+\*" stmt)
      {:spec (from-spec stmt)}

      (re-find #"^export\s+\{" stmt)
      {:spec (from-spec stmt)
       :ops (when-not (re-find #"^export\s+type\b" stmt)
              (brace-names stmt))}

      (re-find #"^export\s+(?:default\s+)?(?:async\s+)?function\b" stmt)
      (when-let [name (second (re-find #"function\s+\*?\s*([A-Za-z_$][\w$]*)" stmt))]
        {:ops [name] :type (return-type header) :value true})

      (re-find #"^export\s+(?:default\s+)?class\b" stmt)
      (when-let [name (second (re-find #"class\s+([A-Za-z_$][\w$]*)" stmt))]
        {:ops [name] :value true})

      (re-find #"^export\s+(?:const|let|var)\b" stmt)
      (when-let [name (second (re-find #"(?:const|let|var)\s+([A-Za-z_$][\w$]*)" stmt))]
        {:ops [name] :type (const-type header) :value true})

      (re-find #"^export\s+(?:default\s+)?interface\b" stmt)
      {:interface true}

      (re-find #"^export\s+(?:default\s+)?enum\b" stmt)
      (when-let [name (second (re-find #"enum\s+([A-Za-z_$][\w$]*)" stmt))]
        {:ops [name] :value true})

      :else nil)))

(defn- import-fact [text at]
  (let [at (at-keyword text at "import")
        stmt (str/triml (subs text at (statement-end text at)))
        spec (or (from-spec stmt) (side-spec stmt))]
    (when spec
      {:spec spec
       :names (vec (concat (brace-names stmt)
                           (keep identity [(default-binding stmt) (star-binding stmt)])))})))

(defn read-module
  "Module surface of TypeScript `source`: import specs, exported names,
  type annotations on those exports, and invoke(\"name\") calls."
  [source]
  (let [{:keys [text holes]} (mask-comments source)
        imports (keep #(import-fact text %) (line-starts text holes "import"))
        exports (keep #(export-fact text %) (line-starts text holes "export"))]
    {:imports (vec imports)
     :exports (vec exports)
     :invokes (vec (distinct (invokes-of text holes)))}))

(defn- module-relative [file root]
  (let [root (.getCanonicalFile (io/file root))
        file (.getCanonicalFile (io/file file))
        rel (str/replace (str (.relativize (.toPath root) (.toPath file))) #"\\" "/")
        no-ext (str/replace rel #"\.(?:[cm]?[jt]sx?)$" "")]
    (-> no-ext
        (str/replace #"(^|/)index$" "$1")
        (str/replace #"/\." ".")
        (str/replace #"/$" "")
        (str/replace "/" "."))))

(defn- ns-join [ns-prefix relative]
  (cond
    (str/blank? relative) (str ns-prefix)
    (str/blank? ns-prefix) relative
    :else (str ns-prefix "." relative)))

(defn- asset? [spec]
  (boolean (re-find #"\.(?:css|scss|sass|less|svg|png|jpe?g|gif|webp|html|md|json)$" spec)))

(defn- resolve-project [file spec path->id]
  (when (and spec (str/starts-with? spec "."))
    (let [base (io/file (.getParentFile (.getCanonicalFile (io/file file))) spec)
          candidates (concat [base]
                             (map #(io/file (str (.getPath base) %)) exts)
                             (map #(io/file base (str "index" %)) exts))]
      (some (fn [c]
              (when (.isFile c)
                (get path->id (.getCanonicalPath c))))
            candidates))))

(defn- foreign-id [spec]
  (-> spec
      (str/replace #"^@" "at.")
      (str/replace #":" ".")
      (str/replace #"/" ".")
      keyword))

(defn- op-maps [names]
  (mapv (fn [name] {:name name :text name}) (distinct names)))

(defn- parse-file [file root path->id prefix ns-prefix]
  (let [surface (read-module (slurp file))
        relative (module-relative file root)
        ns-str (ns-join ns-prefix relative)
        id (graph/id-of ns-str prefix)
        project-bindings (into {}
                               (for [imp (:imports surface)
                                     name (:names imp)
                                     :let [to (resolve-project file (:spec imp) path->id)]
                                     :when to]
                                 [name to]))
        specs (->> (:imports surface)
                   (map :spec)
                   (concat (keep :spec (:exports surface)))
                   (remove str/blank?)
                   (remove asset?)
                   distinct)
        requires (vec (distinct (keep #(resolve-project file % path->id) specs)))
        foreigns (->> specs
                      (remove #(str/starts-with? % "."))
                      (map foreign-id)
                      distinct
                      vec)
        impls (->> (:exports surface)
                   (keep :type)
                   (keep project-bindings)
                   distinct
                   vec)
        ops (op-maps (mapcat :ops (:exports surface)))
        interface? (and (some :interface (:exports surface))
                        (not (some :value (:exports surface))))]
    {:id id
     :name (graph/module-name id)
     :ns ns-str
     :lang :typescript
     :file (graph/relative-path file)
     :ops ops
     :stereotype (when interface? :interface)
     :requires (vec (remove #(= % id) requires))
     :foreigns foreigns
     :impls (vec (remove #(= % id) impls))
     :invokes (:invokes surface)}))

(defn- as-edges [c]
  (concat
    (map (fn [to] {:from (:id c) :to to :kind :dependency}) (:requires c))
    (map (fn [to] {:from (:id c) :to to :kind :dependency}) (:foreigns c))
    (map (fn [to] {:from (:id c) :to to :kind :implements}) (:impls c))))

(defn- foreign-class [id]
  {:id id
   :name (name id)
   :ns (name id)
   :foreign true})

(defn- public-class [c]
  (cond-> (dissoc c :requires :foreigns :impls :invokes)
    (empty? (:ops c)) (dissoc :ops)
    (nil? (:stereotype c)) (dissoc :stereotype)))

(defrecord TypeScriptGraph []
  graph/LanguageGraph
  (scan [_ root opts]
    (let [prefix (or (:prefix opts) "app")
          ns-prefix (or (:ns-prefix opts) prefix)
          files (source-files root)
          path->id (into {}
                         (map (fn [file]
                                (let [ns-str (ns-join ns-prefix (module-relative file root))]
                                  [(.getCanonicalPath file) (graph/id-of ns-str prefix)]))
                              files))
          parsed (mapv #(parse-file % root path->id prefix ns-prefix) files)
          project-ids (set (map :id parsed))
          foreigns (->> parsed
                        (mapcat :foreigns)
                        distinct
                        (remove project-ids)
                        (mapv foreign-class))
          classes (into (mapv public-class parsed) foreigns)
          edges (->> (mapcat as-edges parsed)
                     (remove #(= (:from %) (:to %)))
                     distinct
                     vec)
          invokes (into {} (keep (fn [c]
                                   (when (seq (:invokes c))
                                     [(:id c) (:invokes c)]))
                                 parsed))]
      {:classes classes
       :edges edges
       :invokes invokes})))

(def impl (->TypeScriptGraph))

(graph/register! :typescript impl)
