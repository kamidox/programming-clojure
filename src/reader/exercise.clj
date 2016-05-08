(ns reader.exercise
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

; Protocol
; Protocol is used to solve "expression problem". With clojure protocol, we can
; extend exist type to implement a protocol.
; use `defprotocol` to define a protocol
(defprotocol Greeting
  (say-hi [this]))

; define a type/record to implement a protocol
(deftype Person [first-name last-name]
  Greeting
  (say-hi [this] (str first-name ": Say Hi!")))

; (say-hi (Person. "Joey" "Huang"))
; "Joey: Say Hi!"

; define a record to implement a protocol
(defrecord Cat [name color]
  Greeting 
  (say-hi [this] (str (:name this) ": miao miao ...")))

; (say-hi (Cat. "Cherry" "White"))
; "Cherry: miao miao ..."

; use extend-type to extend a exist type to implement protocol
(extend-type nil
  Greeting
  (say-hi [this] (str "nil say hi here.")))

; (say-hi nil)
; "nill say hi here."

; use extend-protocol to extend multi types in one shot
(extend-protocol Greeting
  String
  (say-hi [this] (str this " say hi here."))
  Number
  (say-hi [this] (str "Number " this " say hi here.")))

; (say-hi "LILY")
; "LILY say hi here."
; (say-hi 5)
; "Number 5 say hi here."

; NOTE!!!
; defprotocol and definterface is different. Please vist folliwing example to inspect:
; http://clojuredocs.org/clojure.core/defprotocol#example-5568ac16e4b03e2132e7d17a

; While deftype and defrecord define named types, `reify` defines both an anonymous type and creates an instance of that type. The use case is where you need a one-off implementation of one or more protocols or interfaces and would like to take advantage of the local context. In this respect it is use case similar to proxy, or anonymous inner classes in Java.

; Using a reified FileFilter implementation to obtain only directory
(defn list-dir [path]
  (.listFiles (java.io.File. path)
              (reify 
                java.io.FileFilter 
                (accept [this f] (.isDirectory f)))))

; (prn (for [f (list-dir "." )] (.getName f)))
; This will ouput all the folder name under current directory

; create a inner class object
(defn reify-greeting []
  (let [r (reify 
            Greeting 
            (say-hi [this] (str "reified object say hi here.")))]
    (say-hi r)))

; (reify-greeting)
; "reified object say hi here."

; Transducers - classical functional programming chain
; http://clojure.org/reference/transducers

; define xform by using comp
(def xf (comp (filter odd?) (map inc)))
; apply transducer: => (reduce f (map inc (filter odd? (range 5))))
(transduce xf conj (range 5))   ; => [2 4]
(transduce xf + (range 5))      ; => 6
(transduce xf + 10 (range 5))   ; => 16
(transduce xf str (range 5))    ; => "24"

; macro
; use defmacro to define a macro named unless
(defmacro unless [condition & forms]
  `(if (not ~condition) ~@forms))

; macroexpand-1 will expand the form for one time
(macroexpand-1 '(unless (= 1 2) "one not equal two" "one equal two? how come?"))

(unless (= 1 2) "one not equal two" "one equal two? how come?")

; define a macro named `bench` which will store the result and time it took to evaluates the result
(defmacro bench [expr]
  `(let [start# (System/nanoTime)
         result# ~expr]
     {:result result# :elapsed (- (System/nanoTime) start#)}))

; (macroexpand-1 '(bench (str "a" "b")))
(bench (str "a" "b"))
