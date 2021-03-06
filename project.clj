(defproject clojure.data.json "1.0.0"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [com.clojure-goes-fast/clj-async-profiler "0.5.0"]
                 [com.clojure-goes-fast/clj-java-decompiler "0.3.0"]
                 [criterium/criterium "0.4.6"]
                 [cheshire/cheshire "5.10.0"]
                 [metosin/jsonista "0.3.1"]]
  :source-paths ["src/main/clojure"]
  :java-source-paths ["src/main/java"]
  :java-test-paths ["src/test/java"]
  :test-paths ["src/test/clojure"]
  :jvm-opts ["-Djdk.attach.allowAttachSelf=true"])

