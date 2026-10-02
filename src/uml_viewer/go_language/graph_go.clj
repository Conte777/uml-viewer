(ns uml-viewer.go-language.graph-go
  "Go LanguageGraph: one class per package; import, implements, and embedding edges."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [uml-viewer.go-language.reader :as reader]
            [uml-viewer.graph :as graph]))

(defn- home-file
  "File the package opens at: doc.go, else <package>.go, else the first file."
  [{:keys [files package]}]
  (let [names (set files)]
    (or (names "doc.go")
        (names (str package ".go"))
        (first (sort files)))))

(defn- foreign-id [import-path]
  (keyword (str/replace import-path "/" ".")))

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
                             (let [d (reader/read-decls (slurp (io/file (:dir pkg) f)))]
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
          pkgs (reader/package-index root prefix ns-prefix)
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
