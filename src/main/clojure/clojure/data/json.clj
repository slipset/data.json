;; Copyright (c) Stuart Sierra, 2012. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns ^{:author "Stuart Sierra"
      :doc "JavaScript Object Notation (JSON) parser/generator.
  See http://www.json.org/"}
  clojure.data.json
  (:refer-clojure :exclude (read))
  (:require [clojure.pprint :as pprint])
  (:import (java.io PrintWriter StringWriter
                    Reader Writer StringReader EOFException)
           [clojure.lang RT]
           [clojure.data.json IBuffer JSONStringBuffer JSONReaderBuffer StackFrame]
           [java.util ArrayList LinkedList]))

;;; JSON READER

(def ^{:dynamic true :private true} *key-fn*)
(def ^{:dynamic true :private true} *value-fn*)

(defn- default-write-key-fn
  [x]
  (cond (instance? clojure.lang.Named x)
        (name x)
        (nil? x)
        (throw (Exception. "JSON object properties may not be nil"))
        :else (str x)))

(defn- default-value-fn [k v] v)

(declare -read)

(defmacro ^:private codepoint [c]
  (int c))

(defn- codepoint-clause [[test result]]
  (cond (list? test)
        [(map int test) result]
        (= test :whitespace)
        ['(9 10 13 32) result]
        (= test :simple-ascii)
        [(remove #{(codepoint \") (codepoint \\) (codepoint \/)}
                 (range 32 127))
         result]
        (= test :js-separators)
        ['(16r2028 16r2029) result]
        :else
        [(int test) result]))

(defmacro ^:private codepoint-case [e & clauses]
  `(case ~e
     ~@(mapcat codepoint-clause (partition 2 clauses))
     ~@(when (odd? (count clauses))
         [(last clauses)])))

(defmacro  codepoint-case [e & clauses]
  `(case ~e
     ~@(mapcat codepoint-clause (partition 2 clauses))
     ~@(when (odd? (count clauses))
         [(last clauses)])))


(set! *warn-on-reflection* true)

(definterface IFoo
  (^int nextChar [])
  (string [])
  (number [bigdec])
  (unread []))
(defn- read-hex-char [^IFoo stream]
  ;; Expects to be called with the head of the stream AFTER the
  ;; initial "\u".  Reads the next four characters from the stream.
  (let [s (str (.nextChar stream)
               (.nextChar stream)
               (.nextChar stream)
               (.nextChar stream))]
    (char (Integer/parseInt s 16))))

(defn- read-escaped-char [^IFoo stream]
  ;; Expects to be called with the head of the stream AFTER the
  ;; initial backslash.
  (let [
        c (.nextChar stream)]
    (codepoint-case (int c)
      (\" \\ \/) (char c)
      \b \backspace
      \f \formfeed
      \n \newline
      \r \return
      \t \tab
      \u (do
           (read-hex-char stream)))))

(defn- slow-read-string [^IFoo stream ^String so-far]
  (let [builder (StringBuilder. so-far)]
    (loop []
      (let [c (.nextChar stream)]
        (cond
          (= (codepoint \") c)
          (str builder)

          (= (codepoint \\) c)
          (do (.append builder (read-escaped-char stream))
              (recur))
          :else
          (do (.append builder (char c))
              (recur)))))))

(defn- read-integer [^String string]
  (if (< (count string) 18)  ; definitely fits in a Long
    (Long/valueOf string)
    (or (try (Long/valueOf string)
             (catch NumberFormatException e nil))
        (bigint string))))

(defn- read-decimal [^String string bigdec]
  (if bigdec
    (bigdec string)
    (Double/valueOf string)))

(defn- fast-string [^chars buffer ^long start]
  (loop [j (int start)]
    (if (= \" (aget buffer j))
      (String. buffer start (unchecked-subtract-int j start))
      (recur (unchecked-inc j)))))

(deftype StringStream [^{:unsynchronized-mutable true :tag int} i
                       ^{:unsynchronized-mutable true :tag int} j
                       ^long buff-size
                       ^chars buffer]
  IBuffer
  (nextChar [_]
    (let [c (aget buffer i)]
      (set! i (unchecked-inc-int i))
      (int c)))
  (string [this]
    ;; Expects to be called with the head of the stream AT the
    ;; opening quotation mark.
    (let [s  "lol"#_(JSONStringBuffer/fastString buffer i)]
      (set! i (unchecked-add-int 1 (unchecked-add-int i (.length s))))
      s)
    #_    (let [i' i]
            (set! j (int i))
            (loop []
              (codepoint-case (int (RT/aget buffer j))
                \" (do
                     (set! i (unchecked-inc-int j))
                     (String. buffer i' (unchecked-subtract-int j i')))
                \\ (do
                     (set! i (int j))
                     (slow-read-string this (String. buffer i' (unchecked-subtract j i'))))
                (do
                  (set! j (unchecked-inc-int j))
                  (recur ))))))

  (keyString [this]
    (loop []
      (let [c (int (RT/aget buffer i))]
        (if (> c (codepoint \ ))
          (if (= c (codepoint \"))
            (do (set! i (unchecked-inc-int i))
                (let [s  "lol" #_(JSONStringBuffer/fastString buffer i)]
                  (set! i (unchecked-add-int 1 (unchecked-add-int i (.length s))))
                  (loop []
                    (let [c (int (RT/aget buffer i))]
                      (if (> c (codepoint \ ))
                        (if (= c (codepoint \:))
                          (do
                            (set! i (unchecked-inc-int i))
                            s)
                          (throw (Exception. "Missing colon in object")))
                        (do
                          (set! i (unchecked-inc-int i))
                          (recur)))))))
            (throw (Exception. "Missing key in object")))
          (do
            (set! i (unchecked-inc-int i))
            (recur))))))

  (number [this bigdec]
    (set! i (int (unchecked-dec i)))
    (loop [decimal? false]
      (let [c (.nextChar this)]
        (codepoint-case c
          (\- \+ \0 \1 \2 \3 \4 \5 \6 \7 \8 \9)
          (if (= i buff-size)
            9
            (recur decimal?))
          (\e \E \.)
          (recur true)
          (do (set! i (int (unchecked-dec i)))
              9
              #_            (let [s (String. (java.util.Arrays/copyOf ^bytes buffer i))]
                              (if decimal?
                                (read-decimal s bigdec)
                                n(read-integer s)))))))))





(defn- read-null [^IBuffer stream]
  (if (= (codepoint \u) (.nextChar stream))
    (if (= (codepoint \l) (.nextChar stream))
      (if (= (codepoint \l) (.nextChar stream))
        nil
        (throw (Exception. "Expected null, got nul")))
      (throw (Exception. "Expected null, got nu")))
    (throw (Exception. "Expected null, got n"))))

(defn- read-true [^IBuffer stream]
  (if (= (codepoint \r) (.nextChar stream))
    (if (= (codepoint \u) (.nextChar stream))
      (if (= (codepoint \e) (.nextChar stream))
        true
        (throw (Exception. "Expected true, got tru")))
      (throw (Exception. "Expected tr")))
    (throw (Exception. "Expected t"))))

(defn- read-false [^IBuffer stream]
  (if (= (codepoint \a) (.nextChar stream))
    (if (= (codepoint \l) (.nextChar stream))
      (if (= (codepoint \s) (.nextChar stream))
        (if (= (codepoint \e) (.nextChar stream))
          false
          (throw (Exception. "Expected false, got fals")))
        (throw (Exception. "Expected false, got fal")))
      (throw (Exception. "Expected false, got fa")))
    (throw (Exception. "Expected false, got f")))
  )



(require '[clj-java-decompiler.core :refer [decompile]])

#_(defprotocol IFrame
  (swap-key [_ k])
  (get-key [_])

  (set-key [_ k])

  (set-value [_ v])

  (set-type [_ t]))

#_(deftype Frame [^{:tag boolean
                  :unsynchronized-mutable true} type
                ^{:tag Object
                  :unsynchronized-mutable true} value
                ^{:tag String
                  :unsynchronized-mutable true} key]
  IFrame
  (swap-key [_ k]
    (let [old key]
      (set! key k)
      old))
  (get-key [_]
    key)

  (set-key [_ k]
    (set! key k))

  (set-value [_ v]
    (set! value v))

  (set-type [_ t]
    (set! type t)))

(int \, )
(defn -read [^JSONStringBuffer stream eof-error? eof-value bigdec key-fn value-fn]
  (let [frame-pool (LinkedList.)]
    (loop [value nil stack (LinkedList.) current-frame nil]
      (let [c (.nextChar stream)]
        (if (> c (codepoint \ ))
          (codepoint-case c
            (\- \0 \1 \2 \3 \4 \5 \6 \7 \8 \9)
            (let [n (.number stream bigdec)]
              (if (not (.isEmpty stack))
                (recur n stack current-frame)
                n))
            \n (do (read-null stream)
                   (if (not (.isEmpty stack)) ;; \f
                     (recur nil stack current-frame)
                     nil))
            \f (do (read-false stream)
                   (if (not (.isEmpty stack)) ;; \f
                     (recur false stack current-frame)
                     false))
            \t (do (read-true stream)
                   (if (not (.isEmpty stack)) ;;\t
                     (recur true stack current-frame)
                     true))
            \" (if (.isEmpty stack)
                 (.string stream)
                 (recur (.string stream) stack current-frame))
            \, (let [v (.value ^StackFrame current-frame)]
                 (if (.type ^StackFrame current-frame)
                   (do
                     (conj! v value)
                     (recur nil
                            stack
                            current-frame))
                   (let [k (.swapKey ^StackFrame current-frame (.keyString stream))]

                     (assoc! v k value)
                     (recur nil
                            stack
                            current-frame))))
            ;; Read JSON arrays
            \[ (let [f (if-let [f ^StackFrame (.pollFirst frame-pool)]
                         (.reset f (transient []))
                         (StackFrame. (transient [])))]
                 (.addFirst stack f)
                 (recur nil stack f))
            \] (let [f ^StackFrame (.pollFirst stack) ;; \]
                     v (.value ^StackFrame f)]
                 (.addFirst frame-pool f)
                 (conj! v value)
                 (if (.isEmpty stack)
                   (persistent! v)
                   (recur (persistent! v) stack (.getFirst stack))))
            \{ (let [k ^String (.keyString stream)
                     f (if-let [f ^StackFrame (.pollFirst frame-pool)]
                         (.reset f (transient {}) k)
                         (StackFrame. (transient {}) k))]
                 (.addFirst stack f)
                 (recur nil stack f))
            \} (let [f ^StackFrame (.pollFirst stack)
                     v (.value ^StackFrame f)]
                 (.addFirst frame-pool f)
                 (assoc! v (.key ^StackFrame f) value)
                 (if (.isEmpty stack)
                   (persistent! v)
                   (recur (persistent! v) stack (.getFirst stack))))

            (throw (Exception.
                    (str "JSON error (unexpected character): `" (char c) "`" (.string stream)))))
          (recur value stack current-frame))))))

#_(let [f ^Frame (.pollFirst stack)
                     v (.value ^Frame f)]
                 (assoc! v (.key ^Frame  f) value)
                 (if (.isEmpty stack)
                   (persistent! v)
                   (recur (unchecked-inc i)
                          (persistent! v)
                          stack
                          (.getFirst stack))))


(int \{)
(defn read
  "Reads a single item of JSON data from a java.io.Reader. Options are
  key-value pairs, valid options are:

     :eof-error? boolean

        If true (default) will throw exception if the stream is empty.

     :eof-value Object

        Object to return if the stream is empty and eof-error? is
        false. Default is nil.

     :bigdec boolean

        If true use BigDecimal for decimal numbers instead of Double.
        Default is false.

     :key-fn function
        Single-argument function called on JSON property names; return
        value will replace the property names in the output. Default
        is clojure.core/identity, use clojure.core/keyword to get
        keyword properties.

     :value-fn function

        Function to transform values in maps (\"objects\" in JSON) in
        the output. For each JSON property, value-fn is called with
        two arguments: the property name (transformed by key-fn) and
        the value. The return value of value-fn will replace the value
        in the output. If value-fn returns itself, the property will
        be omitted from the output. The default value-fn returns the
        value unchanged. This option does not apply to non-map
        collections."
  [reader & options]
  (let [{:keys [eof-error? eof-value bigdec key-fn value-fn]
         :or {bigdec false
              eof-error? true
              key-fn identity
              value-fn default-value-fn}} options]
#_    (-read (PushbackReader. reader) eof-error? eof-value bigdec key-fn value-fn)))

(def buffer (char-array 600001))

(defn read-str
  "Reads one JSON value from input String. Options are the same as for
  read."
  ([^String string]
   (let [#_#_arr (.toCharArray string)]
     (-read (JSONStringBuffer. string)#_(YoloJSONReader. (.toCharArray string) (StringReader. string)) true nil false identity default-value-fn)))
  ([string & options]
   (let [{:keys [eof-error? eof-value bigdec key-fn value-fn]
            :or {bigdec false
                 eof-error? true
                 key-fn identity
                 value-fn default-value-fn}} options]
#_     (-read (PushbackReader. (StringReader. string)) eof-error? eof-value bigdec key-fn value-fn))))

;;; JSON WRITER

(def ^{:dynamic true :private true} *escape-unicode*)
(def ^{:dynamic true :private true} *escape-js-separators*)
(def ^{:dynamic true :private true} *escape-slash*)

(defprotocol JSONWriter
  (-write [object out]
    "Print object to PrintWriter out as JSON"))

(defn- write-string [^CharSequence s ^PrintWriter out]
  (let [sb (StringBuilder. (count s))]
    (.append sb \")
    (dotimes [i (count s)]
      (let [cp (int (.charAt s i))]
        (codepoint-case cp
          ;; Printable JSON escapes
          \" (.append sb "\\\"")
          \\ (.append sb "\\\\")
          \/ (.append sb (if *escape-slash* "\\/" "/"))
          ;; Simple ASCII characters
          :simple-ascii (.append sb (.charAt s i))
          ;; JSON escapes
          \backspace (.append sb "\\b")
          \formfeed  (.append sb "\\f")
          \newline   (.append sb "\\n")
          \return    (.append sb "\\r")
          \tab       (.append sb "\\t")
          ;; Unicode characters that Javascript forbids raw in strings
          :js-separators (if *escape-js-separators*
                           (.append sb (format "\\u%04x" cp))
                           (.appendCodePoint sb cp))
          ;; Any other character is Unicode
          (if *escape-unicode*
            (.append sb (format "\\u%04x" cp)) ; Hexadecimal-escaped
            (.appendCodePoint sb cp)))))
    (.append sb \")
    (.print out (str sb))))

(defn- write-object [m ^PrintWriter out]
  (.print out \{)
  (loop [x m, have-printed-kv false]
    (when (seq x)
      (let [[k v] (first x)
            out-key (*key-fn* k)
            out-value (*value-fn* k v)
            nxt (next x)]
        (when-not (string? out-key)
          (throw (Exception. "JSON object keys must be strings")))
        (if-not (= *value-fn* out-value)
          (do
            (when have-printed-kv
              (.print out \,))
            (write-string out-key out)
            (.print out \:)
            (-write out-value out)
            (when (seq nxt)
              (recur nxt true)))
          (when (seq nxt)
            (recur nxt have-printed-kv))))))
  (.print out \}))

(defn- write-array [s ^PrintWriter out]
  (.print out \[)
  (loop [x s]
    (when (seq x)
      (let [fst (first x)
            nxt (next x)]
        (-write fst out)
        (when (seq nxt)
          (.print out \,)
          (recur nxt)))))
  (.print out \]))

(defn- write-bignum [x ^PrintWriter out]
  (.print out (str x)))

(defn- write-float [^Float x ^PrintWriter out]
  (cond (.isInfinite x)
        (throw (Exception. "JSON error: cannot write infinite Float"))
        (.isNaN x)
        (throw (Exception. "JSON error: cannot write Float NaN"))
        :else
        (.print out x)))

(defn- write-double [^Double x ^PrintWriter out]
  (cond (.isInfinite x)
        (throw (Exception. "JSON error: cannot write infinite Double"))
        (.isNaN x)
        (throw (Exception. "JSON error: cannot write Double NaN"))
        :else
        (.print out x)))

(defn- write-plain [x ^PrintWriter out]
  (.print out x))

(defn- write-null [x ^PrintWriter out]
  (.print out "null"))

(defn- write-named [x out]
  (write-string (name x) out))

(defn- write-generic [x out]
  (if (.isArray (class x))
    (-write (seq x) out)
    (throw (Exception. (str "Don't know how to write JSON of " (class x))))))

(defn- write-ratio [x out]
  (-write (double x) out))

;; nil, true, false
(extend nil                    JSONWriter {:-write write-null})
(extend java.lang.Boolean      JSONWriter {:-write write-plain})

;; Numbers
(extend java.lang.Byte         JSONWriter {:-write write-plain})
(extend java.lang.Short        JSONWriter {:-write write-plain})
(extend java.lang.Integer      JSONWriter {:-write write-plain})
(extend java.lang.Long         JSONWriter {:-write write-plain})
(extend java.lang.Float        JSONWriter {:-write write-float})
(extend java.lang.Double       JSONWriter {:-write write-double})
(extend clojure.lang.Ratio     JSONWriter {:-write write-ratio})
(extend java.math.BigInteger   JSONWriter {:-write write-bignum})
(extend java.math.BigDecimal   JSONWriter {:-write write-bignum})
(extend java.util.concurrent.atomic.AtomicInteger JSONWriter {:-write write-plain})
(extend java.util.concurrent.atomic.AtomicLong    JSONWriter {:-write write-plain})
;; Attempt to support Clojure 1.2.x:
(when-let [class (try (.. Thread currentThread getContextClassLoader
                          (loadClass "clojure.lang.BigInt"))
                      (catch ClassNotFoundException _ false))]
  (extend class JSONWriter {:-write write-bignum}))


;; Symbols, Keywords, and Strings
(extend clojure.lang.Named     JSONWriter {:-write write-named})
(extend java.lang.CharSequence JSONWriter {:-write write-string})

;; Collections
(extend java.util.Map          JSONWriter {:-write write-object})
(extend java.util.Collection   JSONWriter {:-write write-array})

;; Maybe a Java array, otherwise fail
(extend java.lang.Object       JSONWriter {:-write write-generic})

(defn write
  "Write JSON-formatted output to a java.io.Writer. Options are
   key-value pairs, valid options are:

    :escape-unicode boolean

       If true (default) non-ASCII characters are escaped as \\uXXXX

    :escape-js-separators boolean

       If true (default) the Unicode characters U+2028 and U+2029 will
       be escaped as \\u2028 and \\u2029 even if :escape-unicode is
       false. (These two characters are valid in pure JSON but are not
       valid in JavaScript strings.)

    :escape-slash boolean

       If true (default) the slash / is escaped as \\/

    :key-fn function

        Single-argument function called on map keys; return value will
        replace the property names in the output. Must return a
        string. Default calls clojure.core/name on symbols and
        keywords and clojure.core/str on everything else.

    :value-fn function

        Function to transform values in maps before writing. For each
        key-value pair in an input map, called with two arguments: the
        key (BEFORE transformation by key-fn) and the value. The
        return value of value-fn will replace the value in the output.
        If the return value is a number, boolean, string, or nil it
        will be included literally in the output. If the return value
        is a non-map collection, it will be processed recursively. If
        the return value is a map, it will be processed recursively,
        calling value-fn again on its key-value pairs. If value-fn
        returns itself, the key-value pair will be omitted from the
        output. This option does not apply to non-map collections."
  [x ^Writer writer & options]
  (let [{:keys [escape-unicode escape-js-separators escape-slash key-fn value-fn]
         :or {escape-unicode true
              escape-js-separators true
              escape-slash true
              key-fn default-write-key-fn
              value-fn default-value-fn}} options]
    (binding [*escape-unicode* escape-unicode
              *escape-js-separators* escape-js-separators
              *escape-slash* escape-slash
              *key-fn* key-fn
              *value-fn* value-fn]
      (-write x (PrintWriter. writer)))))

(defn write-str
  "Converts x to a JSON-formatted string. Options are the same as
  write."
  [x & options]
  (let [sw (StringWriter.)]
    (apply write x sw options)
    (.toString sw)))

;;; JSON PRETTY-PRINTER

;; Based on code by Tom Faulhaber

(defn- pprint-array [s] 
  ((pprint/formatter-out "~<[~;~@{~w~^, ~:_~}~;]~:>") s))

(defn- pprint-object [m]
  ((pprint/formatter-out "~<{~;~@{~<~w:~_~w~:>~^, ~_~}~;}~:>") 
   (for [[k v] m] [(*key-fn* k) v])))

(defn- pprint-generic [x]
  (if (.isArray (class x))
    (pprint-array (seq x))
    ;; pprint proxies Writer, so we can't just wrap it
    (print (with-out-str (-write x (PrintWriter. *out*))))))

(defn- pprint-dispatch [x]
  (cond (nil? x) (print "null")
        (instance? java.util.Map x) (pprint-object x)
        (instance? java.util.Collection x) (pprint-array x)
        (instance? clojure.lang.ISeq x) (pprint-array x)
        :else (pprint-generic x)))

(defn pprint
  "Pretty-prints JSON representation of x to *out*. Options are the
  same as for write except :value-fn, which is not supported."
  [x & options]
  (let [{:keys [escape-unicode escape-slash key-fn]
         :or {escape-unicode true
              escape-slash true
              key-fn default-write-key-fn}} options]
    (binding [*escape-unicode* escape-unicode
              *escape-slash* escape-slash
              *key-fn* key-fn]
      (pprint/with-pprint-dispatch pprint-dispatch
        (pprint/pprint x)))))

(load "json_compat_0_1")

;; Local Variables:
;; mode: clojure
;; eval: (define-clojure-indent (codepoint-case (quote defun)))
;; End:
