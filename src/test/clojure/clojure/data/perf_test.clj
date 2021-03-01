(ns clojure.data.perf-test
  (:require [criterium.core :refer :all]
            [clojure.data.json :as json]
            [clj-async-profiler.core :as prof]
            [cheshire.core :as cheshire]
            [clojure.string :as str])
  (:import [java.io StringReader]
           [java.util Arrays]))


(def nothing (cheshire/encode nil))

(def still-not-much (cheshire/encode (repeat 100 nil)))

(def whole-lot-of-nothing (cheshire/encode (repeat 100000 nil)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn simple-parse-string-bool-vec-json [^String s]
  (let [result (transient [])]
    (loop [i 0]
      (case (.charAt s i)
        \[ (recur (inc i))
        \, (recur (inc i))
        \] (persistent! result)
        (recur (do
                 (conj! result nil)
                 (+ i 4)))))))

(comment
  (simple-parse-string-bool-vec-json "[null,null]")
  )


(defmacro profiling [times & body]
  `(try
     (prof/start {})
     (dotimes [_# ~times]
       ~@body)
     (finally 
       (println (str "file://" (:path (bean (prof/stop {}))))))))

(defmacro codepoint [x]
  (int x))

(defn simple-parse-bytes-bool-vec-json [^String s]
  (let [result (transient [])
        bts (.getBytes s)]
    (loop [i 0]
      (case (aget bts i)
        91 (recur (inc i))
        44 (recur (inc i))
        93 (persistent! result)
        (recur (do
                 (conj! result nil)
                 (+ i 4)))))))


(comment
  (simple-parse-bytes-bool-vec-json "[null,null]")
  )


(defn checking-parse-bytes-bool-vec-json [^String s]
  (let [result (transient [])
        bts (.getBytes s)]
    (loop [i 0]
      (case (aget bts i)
        91 (recur (inc i))
        44 (recur (inc i))
        93 (persistent! result)
        (recur (do
                 (conj! result nil)
                 (+ i 4)))))))


(comment
  (checking-parse-bytes-bool-vec-json "[null,null]")
 ) 

(defn checking-parse-bytes-bool-vec-json [^String s]
  (let [result (transient [])
        bts (.getBytes s)]
    (loop [i 0]
      (case (aget bts i)
        91 (recur (unchecked-add-int i 1))
        44 (recur (unchecked-add-int i 1))
        93 (persistent! result)
        (if (and (= 117 (aget bts (unchecked-add-int i 1)))
                 (= 108 (aget bts (unchecked-add-int i 2)))
                 (= 108 (aget bts (unchecked-add-int 1 3))))
          (do
            (conj! result nil)
            (recur (unchecked-add-int i 4)))
          (println "fail"))))))
(comment
  (checking-parse-bytes-bool-vec-json "[null,null]")
  )


(defn simple-parse-chars-bool-vec-json [^String s]
  (let [result (transient [])
        bts (.toCharArray s)]
    (loop [i 0]
      (case (aget bts i)
        \[(recur (inc i))
        \, (recur (inc i))
        \] (persistent! result)
        (recur (do
                 (conj! result nil)
                 (+ i 4)))))))

(comment
  (simple-parse-chars-bool-vec-json "[null,null]")
  )

(defn simple-parse-stream-bool-vec-json [^String s]
  (let [result (transient [])
        bts (StringReader. s)]
    (loop []
      (case (.read bts)
       91 (recur)
       44 (recur)
       93 (persistent! result)
       (do
         (.read bts)
         (.read bts)
         (.read bts)
         (conj! result nil)
         (recur)
          #_                 (+ i 4))))))

(comment
  (simple-parse-stream-bool-vec-json "[null,null]")
  )

(defn checking-parse-stream-bool-vec-json [^String s]
  (let [result (transient [])
        bts (StringReader. s)]
    (loop []
      (case (.read bts)
       91 (recur)
       44 (recur)
       93 (persistent! result)
       110 (do
             (if (and (= 117 (.read bts))
                      (= 108 (.read bts))
                      (= 108 (.read bts)))
               (do
                 (conj! result nil)
                 (recur))
               (println "fail")
               ))))))

(comment
  (checking-parse-stream-bool-vec-json "[null,null]")
  (json/read-str "[null,null]")
  )

(char 110)
(comment
  (set! *unchecked-math* true)
  (profiling 1000 (cheshire/parse-string-strict whole-lot-of-nothing))

  (count whole-lot-of-nothing)

  (profiling 10000 (json/-read chrs nil nil nil nil nil))

  (quick-bench (json/-read chrs nil nil nil nil nil)) ;;2.462499 ms

  (profiling 5000 (json/read-str whole-lot-of-nothing))
  (profiling 100000 (simple-parse-string-bool-vec-json whole-lot-of-nothing))

  (profiling 100000 (simple-parse-bytes-bool-vec-json whole-lot-of-nothing))
  (profiling 100000 (checking-parse-bytes-bool-vec-json whole-lot-of-nothing))

  (profiling 100000 (simple-parse-chars-bool-vec-json whole-lot-of-nothing))

  (profiling 100000 (simple-parse-stream-bool-vec-json whole-lot-of-nothing))
  (profiling 100000 (checking-parse-stream-bool-vec-json whole-lot-of-nothing))

  (quick-bench (cheshire/parse-string-strict whole-lot-of-nothing)) ;; 2.672629 ms


  (quick-bench (json/read-str whole-lot-of-nothing)) ;;2.462499 ms

  (= (json/read-str whole-lot-of-nothing) (cheshire/parse-string-strict whole-lot-of-nothing))

  (quick-bench (simple-parse-string-bool-vec-json whole-lot-of-nothing)) ;; 1.511900 ms

  (quick-bench (simple-parse-bytes-bool-vec-json whole-lot-of-nothing)) ;; 1.317282 ms
  (quick-bench (checking-parse-bytes-bool-vec-json whole-lot-of-nothing)) ;; 1.443640 ms 

  (quick-bench (simple-parse-chars-bool-vec-json whole-lot-of-nothing)) ;; 1.711400 ms

  (quick-bench (simple-parse-stream-bool-vec-json whole-lot-of-nothing)) ;; 1.878283 ms
  (quick-bench (checking-parse-stream-bool-vec-json whole-lot-of-nothing)) ;; 1.776490 ms
  )

(def whole-lot-of-stringly-nothing (cheshire/encode (repeat 100000 "null")))
(defn simple-parse-bytes-string-vec-json [^String s]
  (let [result (transient [])
        bts (.getBytes s)]
    (loop [i 0]
      (case (aget bts i)
        91 (recur (unchecked-add-int i 1))
        44 (recur (unchecked-add-int i 1))
        93 (persistent! result)
        34 (recur (loop [j (unchecked-add-int i 1)]
                    (if (= 34 (aget bts j))
                      (do (conj! result "null")
                          (unchecked-add-int j 1))
                      (recur (unchecked-add-int j 1)))))))))
(comment
  (simple-parse-bytes-string-vec-json "[\"null\",\"null\"]")
  )


(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)


(defmacro array-start []
  (int \[))

(defmacro array-end []
  (int \[))

(defmacro string-start []
  (int \"))

(defmacro string-end []
  (int \"))

(defmacro backslash []
  (int \\))

(defmacro object-start []
  (int \{))

(defmacro object-end []
  (int \}))

(defmacro comma []
  (int \,))

(defmacro colon []
  (int \:))

(defmacro digit []
  (map int [\0 \1 \2 \3 \4 \5 \6 \7 \8 \9]))

(defmacro exponent []
  (map int [\e \E]))

(defmacro start-number []
  (map int [\- \+ \0 \1 \2 \3 \4 \5 \6 \7 \8 \9]))

(defn read-string* [result ^chars bts i]
  (loop [j (unchecked-add-int i 1)]
    (if (= \" (aget bts j))
      (do (conj! result (String. bts (unchecked-add-int i 1) (unchecked-subtract-int (unchecked-subtract-int j i) 1)))
          (unchecked-add-int j 1))
      (recur (unchecked-add-int j 1)))))

(defn ok-parse-bytes-string-vec-json [^String s]
  (let [result (transient [])
        bts (.toCharArray s)]
    (loop [i 0]
      (case (aget bts i)
        \[ (recur (unchecked-add-int i 1))
        \  (recur (unchecked-add-int i 1))
        \, (recur (unchecked-add-int i 1))
        \] (persistent! result)
        \" (recur (read-string* result bts i))))))
(comment
  (ok-parse-bytes-string-vec-json "[\"null\",\"nuøl\"]")

  (json/read-str "[\"null\",\"nuøl\"]")
  (json/read-str "{\"lol\":9}")
  )

(comment
  (def json (slurp "https://raw.githubusercontent.com/metosin/jsonista/f2afb123b770fd9af1a8766f385b5e77b9f483e7/dev-resources/json100k.json"))
  (profiling 10000 (cheshire/parse-string-strict whole-lot-of-stringly-nothing))

  (profiling 10000 (json/read-str whole-lot-of-stringly-nothing))

  (profiling 100000 (json/read-str "[\"null\",\"null\",\"null\"]"))

  (quick-bench (json/read-str "[\"null\",\"null\",\"null\"]"))

  (quick-bench (cheshire/parse-string-strict "[\"null\",\"null\",\"null\"]"))

  (def lol (json/read-str whole-lot-of-stringly-nothing))

  (profiling 100000 (simple-parse-bytes-string-vec-json whole-lot-of-stringly-nothing))

  (profiling 1000 (ok-parse-bytes-string-vec-json whole-lot-of-stringly-nothing))

  (quick-bench (cheshire/parse-string-strict whole-lot-of-stringly-nothing)) ;; 4.731242 ms

  (quick-bench (json/read-str whole-lot-of-stringly-nothing)) ;; 11e.905946 ms

  (= (cheshire/parse-string-strict whole-lot-of-stringly-nothing)
     (json/read-str whole-lot-of-stringly-nothing))

  (quick-bench (simple-parse-bytes-string-vec-json whole-lot-of-stringly-nothing)) ;; 2.353805 ms

  (quick-bench (ok-parse-bytes-string-vec-json whole-lot-of-stringly-nothing)) ;; 4.280323 ms

  (json/read-str json)
  (quick-bench (json/read-str json)) ;; 1.441494 ms
  (quick-bench (cheshire/parse-string-strict json)) ;; 907.052560 µs

  (profiling 10000 (json/read-str json))
  (profiling 10000 (cheshire/parse-string-strict json))

  (def objects (cheshire/encode (repeat 100000 "loladfadfadfadfadfafadfadfadfadf")))
  (quick-bench (cheshire/parse-string-strict objects))
  (quick-bench (json/read-str objects))

  (profiling 10000 (json/read-str objects))

  (profiling 10000 (cheshire/parse-string-strict json))
  )


(defn stack [^bytes bytes]
  (loop [i 0
         value nil
         stack []]
    (let [c (aget bytes i)]
      (json/codepoint-case (int c)
        ;; Read numbers
        ;; Read null as nil
        \n (let [skip (unchecked-add i 4)]
             (if-not (empty? stack)
               (recur skip
                      :nil
                      stack)
               :nil))

        \, (let [frame (peek stack)
                 t (:type frame)]
             (if (= t :array)
               (do
                 (conj! (:value frame) value)
                 (recur (unchecked-add i 1)
                        nil
                        stack))
               (if (= t :object)
                 (let [k (:key frame)
                       new-key "bla"]
                   (assoc! frame :key new-key)
                   (assoc! (:value frame) k value)
                   (recur (unchecked-add i 7)
                          nil
                          stack))
                 (throw (Exception. "JSON Error found comma outside of object")))
               #_  (throw (Exception. "JSON Error found comma outside of array"))))
        ;; Read JSON arrays
        \[ (recur (unchecked-add i 1)
                  nil
                  (conj stack (transient {:type :array
                                          :value (transient [])})))
        \] (let [frame (peek stack)
                 new-stack (pop stack)]
             (when value
               (conj! (:value frame) value))
             (if-not (empty? new-stack)
               (recur (unchecked-add i 1)
                      (persistent! (:value frame))
                      new-stack)
               (persistent! (:value (peek stack)))))
        \{ (let [k "lol"]
             (recur
              (unchecked-add i 7)
              nil
              (conj stack (transient {:type :object
                                      :key k
                                      :value (transient {})}))))
        \} (let [frame (peek stack)
                 new-stack (pop stack)]
             (when value
               (assoc! (:value frame) (:key frame) value))
             (if-not (empty? new-stack)
               (recur (unchecked-add i 1)
                      (persistent! (:value frame))
                      new-stack)
               (persistent! (:value frame))))

        (throw (Exception.
                (str "JSON error (unexpected character): " (char c))))))))

(comment
  (println "lol")
  (stack (.getBytes "null"))
  (stack (.getBytes "[null]"))
  (stack (.getBytes "[null,[null]]"))
  (stack (.getBytes "[null,[[null]]]"))
  (stack (.getBytes "[null,[[[null]]]]"))
  (stack (.getBytes "[{\"lol\":null}]"))
  (stack (.getBytes "{\"lol\":null}"))
  (stack (.getBytes "{\"lol\":[null]}"))
  (stack (.getBytes "{\"lol\":null,\"bla\":null}"))
  (stack (.getBytes "{\"lol\":null,\"bla\":{\"lol\":null}}"))
  
  )



(defn string-to-bytes-to-string [^String s]
  (String. ^bytes (.getBytes s)))

(defn string-to-chars-to-string [^String s]
  (String. ^chars (.toCharArray s)))



(defn string->bytes [^ String s]
  (.getBytes s))

(defn string->chars [^String s]
  (.toCharArray s))

(defn bytes->string [^bytes bts]
  (String. bts))

(defn chars->string [^chars bts]
  (String. bts))


(comment
  (def s (str/join "" (range 100)))
  (def bts (.getBytes s))
  (def chrs (.toCharArray s))
  (string-to-bytes-to-string "lol")
  (string-to-chars-to-string "lol")

  (profiling 1000000 (string-to-bytes-to-string s))
  (profiling 1000000 (string-to-chars-to-string s))

  (quick-bench   (string-to-bytes-to-string s)) ;; 59.292395 ns
  (quick-bench   (string-to-chars-to-string s)) ;; 65.318554 ns


  (profiling 1000000 (string->bytes s))
  (profiling 1000000 (string->chars s))


  (quick-bench   (string->bytes s)) ;; 59.292395 ns
  (quick-bench   (string->chars s)) ;; 65.318554 ns


  (profiling 1000000 (bytes->string bts))
  (profiling 1000000 (chars->string chrs))


  (quick-bench   (bytes->string bts))
  (quick-bench   (chars->string chrs)))


(defn read-char [^java.io.Reader s ]
  (.read s))


(defn length [^java.io.Reader s]
  (loop [i 0]
    (let [c (.read s)]
      (if (= -1 c)
        i
        (recur (inc i))))))

(defn length' [^chars chrs ^long cnt]
  (loop [i 0]
    (let [c (aget chrs i)]
      (if (= i cnt)
        i
        (recur (inc i))))))
(comment
  (def string (clojure.string/join (range 10000)))
  (count string)
  (quick-bench (length (java.io.BufferedReader. (StringReader. string) 38)))

  (quick-bench (let [arr (.toCharArray string)
                     cnt (dec (count arr))]
                 (length' arr cnt)))
  (def buffer [char-array 3])



  )


