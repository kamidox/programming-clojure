(ns reader.exploring
  (:use [clojure.java.io :only (reader)])
  (:import (java.io File)))

(defn non-blank?
  "Is this a non-blank line?"
  [line] (if (re-find #"\S" line) true false))

(defn non-svn?
  "Is this not a svn file?"
  [file]
  (not (.contains (.getName file) ".svn")))

(defn clojure-source?
  "Is this a clojure source file?"
  [file]
  (.endsWith (.getName file) ".clj"))

(defn file-loc
  "Calculate Line Of Code of file"
  [file]
  (with-open [rdr (reader file)]
    (count (filter non-blank? (line-seq rdr)))))

(defn loc
  "Calculate Line Of Code of all .clj in dir"
  [dir]
  (let [file-list (filter 
                    #(and (non-svn? %) (clojure-source? %))
                    (file-seq (File. dir)))
        loc-list (map file-loc file-list)]
    (reduce + loc-list)))

(defn clojure-loc
  "Calculate Line Of Code of all clojure source in dir"
  [dir]
  (reduce
    +
    (for [file (file-seq (File. dir))
          :when (and (non-svn? file) (clojure-source? file))]
      (file-loc file))))

