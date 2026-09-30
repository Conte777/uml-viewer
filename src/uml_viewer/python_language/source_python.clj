(ns uml-viewer.python-language.source-python
  "Python LanguageSource: open :file and find a def or class."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [uml-viewer.source :as source])
  (:import [java.util.regex Pattern]))

(def ^:private pattern-fmts
  ["async\\s+def\\s+%s\\b"
   "def\\s+%s\\b"
   "class\\s+%s\\b"])

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
    (let [end (or (str/index-of source \newline start) (count source))]
      (str/trimr (subs source start end)))))

(defn- existing-file [ident]
  (let [f (:file ident)]
    (when (and (seq (str f)) (.isFile (io/file f)))
      (str/replace (str f) #"\\" "/"))))

(defrecord PythonSource []
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

(def impl (->PythonSource))

(source/register! :python impl)
