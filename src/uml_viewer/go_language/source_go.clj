(ns uml-viewer.go-language.source-go
  "Go LanguageSource: a class is a package directory; find a func, method, or type in it."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [uml-viewer.source :as source])
  (:import [java.util.regex Pattern]))

(defn- member-patterns [member-name]
  (let [[owner method] (str/split (str member-name) #"\." 2)]
    (if method
      [(format "(?m)^func\\s*\\(\\s*(?:[A-Za-z_]\\w*\\s+)?\\*?\\s*%s(?:\\[[^\\]]*\\])?\\s*\\)\\s*%s\\b"
               (Pattern/quote owner) (Pattern/quote method))]
      [(format "(?m)^func\\s+%s\\b" (Pattern/quote owner))
       (format "(?m)^type\\s+%s\\b" (Pattern/quote owner))])))

(defn- member-start [source member-name]
  (when (and source (seq (str member-name)))
    (some (fn [pattern]
            (let [m (.matcher (Pattern/compile pattern) source)]
              (when (.find m) (.start m))))
          (member-patterns member-name))))

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

(defn- slashes [path]
  (str/replace (str path) #"\\" "/"))

(defn- package-files [file]
  (->> (.listFiles (.getParentFile (io/file file)))
       (filter #(.isFile %))
       (map #(.getName %))
       (filter #(str/ends-with? % ".go"))
       (remove #(str/ends-with? % "_test.go"))
       sort
       (map #(slashes (.getPath (io/file (.getParentFile (io/file file)) %))))))

(defn- existing-file [ident]
  (let [f (:file ident)]
    (when (and (seq (str f)) (.isFile (io/file f)))
      f)))

(defrecord GoSource []
  source/LanguageSource
  (locate [_ ident]
    (when-let [f (existing-file ident)]
      (if (seq (str (:name ident)))
        (some (fn [path]
                (when (member-start (slurp path) (:name ident)) path))
              (package-files f))
        (slashes f))))
  (extract [_ source ident]
    (extract-member source (:name ident)))
  (start-line [_ source ident]
    (member-line source (:name ident)))
  (title [_ ident]
    (str (or (:file ident) (:ns ident))
         (when (:name ident) (str "/" (:name ident))))))

(def impl (->GoSource))

(source/register! :go impl)
