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

(defn source-file?
  "Is this a source file of ext"
  [file ext]
  (.endsWith (.getName file) ext))

(defn file-loc
  "Calculate Line Of Code of file"
  [file]
  (with-open [rdr (reader file)]
    (count (filter non-blank? (line-seq rdr)))))

(defn loc
  "Calculate Line Of Code of all .clj in dir"
  [dir ext]
  (let [file-list (filter 
                    #(and (non-svn? %) (source-file? % ext))
                    (file-seq (File. dir)))
        loc-list (map file-loc file-list)]
    (reduce + loc-list)))

(defn clojure-loc
  "Calculate Line Of Code of all clojure source in dir"
  [dir]
  (reduce
    +
    (for [file (file-seq (File. dir))
          :when (and (non-svn? file) (source-file? file ".clj"))]
      (file-loc file))))

; functional - lazy seq fibo
(defn lazy-seq-fibo
  "lazy seq fibo. For example (take 10 (lazy-seq-fibo))"
  ([] (concat [0 1] (lazy-seq-fibo 0N 1N)))
  ([a b] 
    (let [n (+ a b)]
      (lazy-seq (cons n (lazy-seq-fibo b n))))))

(defn fibo
  "lazy seq fibo. Final solution.
   Using the definition of iterate."
  []
  (map first (iterate (fn [[a b]] [b (+ a b)]) [0N 1N])))

(defn sum
  "sum up all args. Example: (apply sum [1 2 3 4]) or (sum 1 2 3 4)"
  ([] 0N)
  ([a] (if (nil? a) 0N a))
  ([a & b]
    (letfn [(add [a more] (if more (recur (+ a (first more)) (next more)) a))]
      (add a b)))
  )

(defn sum-inner
  "sum up all args. Example: (apply sum [1 2 3 4]) or (sum 1 2 3 4)"
  ([] 0N)
  ([a] (if (nil? a) 0N a))
  ([a & b]
    ((fn [a more] (if more (recur (+ a (first more)) (next more)) a))
      a b))
  )

(defn sum-builtin
  "sum up all args. Example: (apply sum [1 2 3 4]) or (sum 1 2 3 4).
   Implement by built-in function reduce"
  [& a] (reduce + a))

