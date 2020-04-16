(ns hash-object
  (:require [clojure.java.io :as io])
  (:import (java.io FileReader ByteArrayInputStream ByteArrayOutputStream File)
           (java.util.zip DeflaterOutputStream)
           (java.security MessageDigest)))

(defn sha1-hash-bytes [data]
  (.digest (MessageDigest/getInstance "sha1")
           (.getBytes data)))

(defn byte->hex-digits [byte]
  (format "%02x"
          (bit-and 0xff byte)))

(defn bytes->hex-string [bytes]
  (->> bytes
       (map byte->hex-digits)
       (apply str)))

(defn sha1-sum [header+blob]
  (bytes->hex-string (sha1-hash-bytes header+blob)))

(defn zip-str
  "Zip the given data with zlib. Return a ByteArrayInputStream of the zipped
  content."
  [data]
  (let [out (ByteArrayOutputStream.)
        zipper (DeflaterOutputStream. out)]
    (io/copy data zipper)
    (.close zipper)
    (ByteArrayInputStream. (.toByteArray out))))

(defn make-address [x]
  (println (sha1-sum (str "blob " (count (slurp (FileReader. x))) "\0" (slurp (FileReader. x))))))

(defn make-blob [x dir db]
  (let [header+blob (str "blob " (count (slurp (FileReader. x))) "\0" (slurp (FileReader. x)))
        sha1 (sha1-sum header+blob)
        path (str dir "/" db "/objects/")]
    (println sha1)
    (.mkdir (File. (str path (subs sha1 0 2))))
    (io/copy (zip-str header+blob) (io/file (str path (subs sha1 0 2) "/" (subs sha1 2))))))

(defn hash-object [x, dir, db]
  (cond
    (or (= (first x) "--help") (= (first x) "-h")) (println "idiot hash-object: compute address and maybe create blob from file\n\nUsage: idiot hash-object [-w] <file>\n\nArguments:\n   -h       print this message\n   -w       write the file to database as a blob object\n   <file>   the file")
    (= (.exists (io/as-file (str dir "/" db))) false) (println "Error: could not find database. (Did you run `idiot init`?)")
    (= (count x) 0) (println "Error: you must specify a file.")
    (= (first x) "-w")
    (cond
      (= (count x) 1) (println "Error: you must specify a file.")
      :else (make-blob (str dir "/" (first (rest x))) dir db))
    (= (.exists (io/as-file (str dir "/" (first x)))) false) (println "Error: that file isn't readable")
    (= (count x) 1) (make-address (str dir "/" (first x)))))
