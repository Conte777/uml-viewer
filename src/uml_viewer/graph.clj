(ns uml-viewer.graph
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defprotocol LanguageGraph
  (scan [this root opts]
    "Project graph of classes and edges from source under `root`.
     `opts` is a map; `:prefix` is the project namespace prefix.
     Returns `{:classes [{:id :name :ns :stereotype :foreign}]
               :edges [{:from :to :kind}]}`.
     External requires are classes with `:foreign true`.
     A scan may also return `:commands` and `:invokes` for `merge-scans`."))

(defonce ^:private languages (atom {}))

(defn register!
  "Install `impl` as the graph scanner for `lang` (e.g. `:clojure`)."
  [lang impl]
  (swap! languages assoc lang impl)
  lang)

(defn lookup
  [lang]
  (get @languages lang))

(defn scan-project
  "Scan `root` with the registered scanner for `lang` (default `:clojure`)."
  ([root] (scan-project :clojure root {}))
  ([lang-or-root root-or-opts]
   (if (map? root-or-opts)
     (scan-project :clojure lang-or-root root-or-opts)
     (scan-project lang-or-root root-or-opts {})))
  ([lang root opts]
   (if-let [impl (lookup lang)]
     (scan impl root opts)
     (throw (ex-info (str "no LanguageGraph for " lang) {:lang lang})))))

(defn id-of
  "Keyword id of namespace `ns-str` with `prefix` removed.
  `demo.a` with prefix `demo` is `:a`. A name equal to the prefix stays whole."
  [ns-str prefix]
  (let [prefix (str (or prefix ""))
        dotted (str prefix ".")]
    (keyword
      (cond
        (str/blank? prefix) (str ns-str)
        (str/starts-with? (str ns-str) dotted) (subs (str ns-str) (count dotted))
        :else (str ns-str)))))

(defn module-name
  "Last segment of `id`. Hyphen-words are capitalized.
  A segment that already contains a capital keeps the rest of its spelling."
  [id]
  (let [seg (last (str/split (name (keyword id)) #"\."))]
    (->> (str/split seg #"-")
         (remove str/blank?)
         (map (fn [part]
                (if (re-find #"[A-Z]" part)
                  (str (str/upper-case (subs part 0 1)) (subs part 1))
                  (str/capitalize part))))
         (str/join))))

(defn relative-path
  "Path of `file` relative to the working directory, with forward slashes."
  [file]
  (let [path (-> (.getCanonicalPath (io/file file))
                 (str/replace #"\\" "/"))
        root (-> (.getCanonicalPath (io/file (or (System/getProperty "user.dir") ".")))
                 (str/replace #"\\" "/"))
        prefix (str root "/")]
    (if (str/starts-with? path prefix)
      (subs path (count prefix))
      path)))

(defn merge-scans
  "Combine language scans into one graph.
  Each scan is `{:classes :edges :commands :invokes}`.
  `:commands` maps a class id to command names. `:invokes` maps a class id
  to names it calls. A call becomes a `:dependency` on the command's class."
  [scans]
  (let [scans (vec scans)
        classes (mapcat :classes scans)
        by-id (group-by :id classes)
        dup (->> by-id
                 (keep (fn [[id cs]]
                         (when (> (count (remove :foreign cs)) 1) id)))
                 first)]
    (when dup
      (throw (ex-info (str "duplicate class id: " dup) {:id dup})))
    (let [classes (mapv (fn [id]
                          (let [cs (get by-id id)]
                            (or (first (remove :foreign cs))
                                (first cs))))
                        (distinct (map :id classes)))
          owners (into {}
                       (for [scan scans
                             [id cmds] (:commands scan)
                             cmd cmds]
                         [cmd id]))
          linked (for [scan scans
                       [id names] (:invokes scan)
                       name names
                       :let [to (get owners name)]
                       :when (and to (not= to id))]
                   {:from id :to to :kind :dependency})
          edges (->> (concat (mapcat :edges scans) linked)
                     distinct
                     vec)]
      {:classes classes :edges edges})))
