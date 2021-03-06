(ns clojure.data.json-perf-test
  (:require [cheshire.core :as cheshire]
            [clj-async-profiler.core :as prof]
            [clojure.data.json :as json]
            [jsonista.core :as jsonista]
            [criterium.core :refer :all])
  (:import [com.jsoniter JsonIterator]))

(defmacro profiling [times & body]
  `(try
     (prof/start {})
     (dotimes [_# ~times]
       ~@body)
     (finally
       (println (str "file://" (:path (bean (prof/stop {}))))))))

(defn json-data [size]
  (slurp (str "dev-resources/json" size ".json")))

(defn do-bench [size]
  (let [json (json-data size)]
    (println "Results for"  size "json:")
    (println "data.json:")
    (println (with-out-str (quick-bench (json/read-str json))))
    (println "cheshire:")
    (println (with-out-str (quick-bench (cheshire/parse-string-strict json))))
    (println "jsonista:")
    (println (with-out-str (quick-bench (jsonista/read-value json))))
    (println "jsoniter:")
    (println (with-out-str (quick-bench (.read (JsonIterator/parse ^String json)))))))

(defn bench-all-sizes []
  (doseq [size ["10b" "100b" "1k" "10k" "100k"]]
    (do-bench size)))

(defn bench-data-json []
  (doseq [size ["10b" "100b" "1k" "10k" "100k"]]
    (let [data (json-data size)]
      (quick-bench (json/read-str data)))))

(defn profile-all-sizes []
  (doseq [size ["10b" "100b" "1k" "10k" "100k"]]
    (let [json (json-data size)]
      (profiling 10000 (json/read-str json))
      (Thread/sleep 1000))))

(defn bla []
  (.read
    (JsonIterator/parse "{\"loladfadsfadfalkjkjalksdfadsfadsfadfakljlkjlkjasdfadfadfadf\":\"bla\"}")))

(bla)
