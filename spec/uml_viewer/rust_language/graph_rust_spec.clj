(ns uml-viewer.rust-language.graph-rust-spec
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [speclj.core :refer :all]
            [uml-viewer.graph :as graph]
            [uml-viewer.rust-language.graph-rust :as rust]))

(defn- spit-file [dir rel content]
  (let [f (io/file dir rel)]
    (io/make-parents f)
    (spit f content)
    f))

(defn- temp-crate []
  (doto (io/file (System/getProperty "java.io.tmpdir")
                 (str "uml-rs-" (System/nanoTime)))
    io/make-parents))

(describe "rust module surface"
  (it "reads uses, mods, commands, and public fns"
    (let [surface (rust/read-module
                    (str "use serde::Serialize;\n"
                         "use std::fs;\n"
                         "mod proto;\n"
                         "#[tauri::command]\n"
                         "fn read_text(path: String) -> Result<String, String> { Ok(path) }\n"
                         "pub fn run() {}\n"
                         "fn hidden() {}\n"))]
      (should= ["serde::Serialize" "std::fs"] (:uses surface))
      (should= ["proto"] (:mods surface))
      (should= ["read_text"] (:commands surface))
      (should= ["read_text" "run"] (:ops surface)))))

(describe "rust graph"
  (it "scans the crate, links the binary to the lib, and keeps commands"
    (let [dir (temp-crate)]
      (spit-file dir "Cargo.toml"
                 "[package]\nname = \"bookwriter\"\nversion = \"0.1.0\"\n\n[lib]\nname = \"bookwriter_lib\"\n")
      (spit-file dir "src/lib.rs"
                 (str "use tauri::Manager;\n"
                      "mod proto;\n"
                      "#[tauri::command]\n"
                      "fn read_text(path: String) -> String { path }\n"
                      "pub fn run() {}\n"))
      (spit-file dir "src/proto.rs" "pub trait Q {}\n")
      (spit-file dir "src/main.rs" "fn main() { bookwriter_lib::run(); }\n")
      (spit-file dir "src/ignored.rs" "pub fn nope() {}\n")
      (let [g (graph/scan (graph/lookup :rust) (io/file dir "src")
                          {:prefix "bookwriter" :ns-prefix "bookwriter.rust"})
            by-id (into {} (map (juxt :id identity) (:classes g)))
            edges (set (map (juxt :from :to :kind) (:edges g)))]
        (should= #{:rust :rust.proto :rust.main :tauri}
                 (set (keys by-id)))
        (should= "bookwriter.rust" (:ns (by-id :rust)))
        (should= "Rust" (:name (by-id :rust)))
        (should= :rust (:lang (by-id :rust)))
        (should (str/ends-with? (:file (by-id :rust)) "src/lib.rs"))
        (should= ["read_text" "run"] (map :name (:ops (by-id :rust))))
        (should (:foreign (by-id :tauri)))
        (should (contains? edges [:rust :rust.proto :dependency]))
        (should (contains? edges [:rust :tauri :dependency]))
        (should (contains? edges [:rust.main :rust :dependency]))
        (should= ["read_text"] (get-in g [:commands :rust]))
        (should-not (contains? (set (keys by-id)) :rust.ignored)))))

  (it "records impl Trait as implements"
    (let [dir (temp-crate)]
      (spit-file dir "Cargo.toml" "[lib]\nname = \"demo_lib\"\n")
      (spit-file dir "src/lib.rs" "mod proto;\nmod rec;\n")
      (spit-file dir "src/proto.rs" "pub trait Q {}\n")
      (spit-file dir "src/rec.rs"
                 (str "use crate::proto::Q;\n"
                      "struct S;\n"
                      "impl Q for S {}\n"))
      (let [g (graph/scan (graph/lookup :rust) (io/file dir "src")
                          {:prefix "demo" :ns-prefix "demo.rust"})
            edges (set (map (juxt :from :to :kind) (:edges g)))]
        (should (contains? edges [:rust.rec :rust.proto :implements]))
        (should (contains? edges [:rust.rec :rust.proto :dependency]))))))
