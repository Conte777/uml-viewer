(ns uml-viewer.rust-language.source-rust
  "Rust LanguageSource: open :file and find a fn, struct, or trait."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [uml-viewer.source :as source])
  (:import [java.util.regex Pattern]))

(def ^:private pattern-fmts
  ["#\\[\\s*tauri\\s*::\\s*command[^\\]]*\\]\\s*(?:pub(?:\\([^)]*\\))?\\s+)?(?:async\\s+)?fn\\s+%s\\b"
   "pub(?:\\([^)]*\\))?\\s+async\\s+fn\\s+%s\\b"
   "pub(?:\\([^)]*\\))?\\s+fn\\s+%s\\b"
   "async\\s+fn\\s+%s\\b"
   "fn\\s+%s\\b"
   "pub(?:\\([^)]*\\))?\\s+(?:struct|enum|trait|type|const|static)\\s+%s\\b"
   "(?:struct|enum|trait|type|const|static)\\s+%s\\b"])

(defn- member-start [source member-name]
  (when (and source member-name)
    (let [quoted (Pattern/quote (str member-name))]
      (some (fn [fmt]
              (let [m (.matcher (Pattern/compile (format fmt quoted)) source)]
                (when (.find m) (.start m))))
            pattern-fmts))))

(defn member-line
  "1-based line of `member-name` in `source`, or nil."
  [source member-name]
  (when-let [start (member-start source member-name)]
    (inc (count (re-seq #"\n" (subs source 0 start))))))

(defn extract-member
  "Declaration line of `member-name`, or nil."
  [source member-name]
  (when-let [start (member-start source member-name)]
    (let [name-at (or (str/index-of source (str member-name) start) start)
          end (or (str/index-of source \newline name-at) (count source))]
      (str/trimr (subs source start end)))))

(defn- existing-file [ident]
  (let [f (:file ident)]
    (when (and (seq (str f)) (.isFile (io/file f)))
      (str/replace (str f) #"\\" "/"))))

(defrecord RustSource []
  source/LanguageSource
  (locate [_ ident]
    (existing-file ident))
  (extract [_ source ident]
    (extract-member source (:name ident)))
  (start-line [_ source ident]
    (member-line source (:name ident)))
  (title [_ ident]
    (str (or (:file ident) (:ns ident))
         (when (:name ident) (str "/" (:name ident))))))

(def impl (->RustSource))

(source/register! :rust impl)
