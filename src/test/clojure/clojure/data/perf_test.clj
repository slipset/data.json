(ns clojure.data.perf-test
  (:require [criterium.core :refer :all]
            [clojure.data.json :as json]
            [clj-async-profiler.core :as prof]
            [cheshire.core :as cheshire]
            [clojure.string :as str]
            [criterium.core :refer :all])
  (:import [java.io StringReader]
           [java.util Arrays]))

(defmacro profiling [times & body]
  `(try
     (prof/start {})
     (dotimes [_# ~times]
       ~@body)
     (finally 
       (println (str "file://" (:path (bean (prof/stop {}))))))))

(def whole-lot-of-stringly-nothing (cheshire/encode (repeat 100000 "null")))


(def json (slurp "https://raw.githubusercontent.com/metosin/jsonista/f2afb123b770fd9af1a8766f385b5e77b9f483e7/dev-resources/json100k.json"))
(comment

  (profiling 10000 (cheshire/parse-string-strict whole-lot-of-stringly-nothing))

  (profiling 10000 (json/read-str whole-lot-of-stringly-nothing))

  (profiling 100000 (json/read-str "[\"null\",\"null\",\"null\"]"))

  (quick-bench (json/read-str "[\"null\",\"null\",\"null\"]"))

  (quick-bench (cheshire/parse-string-strict "[\"null\",\"null\",\"null\"]"))


  (quick-bench (cheshire/parse-string-strict whole-lot-of-stringly-nothing)) ;; 4.731242 ms

  (quick-bench (json/read-str whole-lot-of-stringly-nothing)) ;; 11e.905946 ms

  (= (cheshire/parse-string-strict whole-lot-of-stringly-nothing)
     (json/read-str whole-lot-of-stringly-nothing))


  (json/read-str json)
  (quick-bench (json/read-str json)) ;; 1.441494 ms
  (quick-bench (cheshire/parse-string-strict json)) ;; 907.052560 µs
  
  (dotimes [_ 10000]
    (json/read-str json))

  
  (profiling 10000 (json/read-str json))
  (profiling 10000 (cheshire/parse-string-strict json))
  

  (def strings (cheshire/encode (repeat 100000 "loladfadfadfadfadfafadfadfadfadf")))
  (quick-bench (cheshire/parse-string-strict strings))
  (quick-bench (json/read-str strings))

  (profiling 10000 (json/read-str strings))


  (def objects (cheshire/encode (repeat 100000 {"loladfadfadfadfadfafadfadfadfadf" 9})))

  (dotimes [_ 10000]
    (json/read-str objects))


  (quick-bench (cheshire/parse-string-strict objects))
  (quick-bench (json/read-str objects))

  (profiling 1000 (cheshire/parse-string-strict objects))
  (profiling 1000 (json/read-str objects))
  )

