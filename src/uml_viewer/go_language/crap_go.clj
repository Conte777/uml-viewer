(ns uml-viewer.go-language.crap-go
  "CRAP for Go packages: coverage from go test, complexity counted per function."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [uml-viewer.go-language.reader :as reader]))

(defn- body-open
  "Index of the `{` that opens the body of the func declared at `pos`.
  Braces of `interface{}` and `struct{}` in the signature are skipped."
  [^String text pos]
  (let [n (count text)]
    (loop [j pos depth 0]
      (when (< j n)
        (let [c (.charAt text j)]
          (cond
            (#{\( \[} c) (recur (inc j) (inc depth))
            (#{\) \]} c) (recur (inc j) (dec depth))
            (and (= c \{) (re-find #"(?:interface|struct)\s*$" (subs text pos j)))
            (recur (reader/close-of text j) depth)
            (and (= c \{) (zero? depth)) j
            :else (recur (inc j) depth)))))))

(defn- complexity [body]
  (inc (count (re-seq #"\b(?:if|for|case)\b|&&|\|\|" body))))

(defn- line-at [^String text pos]
  (inc (count (re-seq #"\n" (subs text 0 pos)))))

(defn- functions
  "`{:name :line :complexity}` of each function and method with a body in `source`."
  [source]
  (let [text (reader/mask-comments source true)
        {:keys [funcs methods]} (reader/read-decls source)]
    (->> (concat (map #(assoc % :label (:name %)) funcs)
                 (map #(assoc % :label (str (:recv %) "." (:name %))) methods))
         (keep (fn [{:keys [label pos]}]
                 (when-let [open (body-open text pos)]
                   {:name label
                    :line (line-at text pos)
                    :complexity (complexity (subs text open (reader/close-of text open)))})))
         vec)))

(defn- run [root & cmd]
  (let [pb (doto (ProcessBuilder. ^java.util.List (vec cmd))
             (.directory (io/file root))
             (.redirectErrorStream true))
        _ (.put (.environment pb) "GOPROXY" "off")
        proc (.start pb)
        out (slurp (.getInputStream proc))]
    {:exit (.waitFor proc) :out out}))

(defn- coverage
  "Percent covered by `[file-import-path line]`. A failing `go test` is echoed to stderr."
  [root]
  (let [profile (java.io.File/createTempFile "uml-go-cover" ".out")]
    (try
      (let [test (run root "go" "test" (str "-coverprofile=" (.getPath profile)) "./...")]
        (when-not (zero? (:exit test))
          (binding [*out* *err*]
            (println "go test failed; coverage is partial:")
            (print (:out test))
            (flush)))
        (if (pos? (.length profile))
          (->> (str/split-lines (:out (run root "go" "tool" "cover" (str "-func=" (.getPath profile)))))
               (keep #(re-matches #"(.+\.go):(\d+):\s+\S+\s+([\d.]+)%" %))
               (into {} (map (fn [[_ file line pct]]
                               [[file (parse-long line)] (parse-double pct)]))))
          {}))
      (finally
        (.delete profile)))))

(defn crap [cc cov]
  (+ (* cc cc (Math/pow (- 1.0 (/ cov 100.0)) 3)) cc))

(defn entries
  "CRAP entries of the Go packages under `root`, keyed by each package's namespace."
  [root prefix ns-prefix]
  (let [rootf (.getCanonicalFile (io/file root))
        covered (coverage rootf)]
    (vec
      (for [pkg (reader/package-index rootf prefix ns-prefix)
            file (:files pkg)
            f (functions (slurp (io/file (:dir pkg) file)))
            :let [cov (get covered [(str (:import-path pkg) "/" file) (:line f)] 0.0)]]
        {:name (:name f)
         :namespace (:ns pkg)
         :complexity (:complexity f)
         :coverage cov
         :crap (crap (:complexity f) cov)}))))

(defn- go-sources
  "Go source trees of `policy`, with the same defaults as the IR generator."
  [policy]
  (let [prefix (or (:prefix policy) "uml-viewer")
        lang-of #(keyword (or (:lang %) (:lang policy) :clojure))]
    (if (seq (:sources policy))
      (for [s (:sources policy) :when (= :go (lang-of s))]
        {:root (or (:root s) (:src s) (:src policy) "src")
         :prefix prefix
         :ns-prefix (or (:prefix s) prefix)})
      (when (= :go (lang-of {}))
        [{:root (or (:src policy) "src") :prefix prefix :ns-prefix prefix}]))))

(defn write!
  "Write CRAP for the Go sources of `policy` into `out`. Entries of other
  namespaces already in `out` are kept."
  [policy out]
  (let [fresh (vec (mapcat #(entries (:root %) (:prefix %) (:ns-prefix %)) (go-sources policy)))
        owned (set (map :namespace fresh))
        old (when (.isFile (io/file out)) (:entries (edn/read-string (slurp out))))
        kept (remove #(owned (:namespace %)) old)]
    (io/make-parents (io/file out))
    (spit out (pr-str {:entries (vec (sort-by (comp - :crap) (concat kept fresh)))}))
    (count fresh)))
