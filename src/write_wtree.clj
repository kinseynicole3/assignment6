(ns write-wtree
  (:require [clojure.java.io :as io])
  (:import (java.security MessageDigest)
           (java.io File ByteArrayOutputStream ByteArrayInputStream)
           (java.util.zip DeflaterOutputStream)))

(defn sha-bytes [bytes]
  (.digest (MessageDigest/getInstance "sha1") bytes))

(defn to-hex-string
  "Convert the given byte array into a hex string, 2 characters per byte."
  [bytes]
  (letfn [(to-hex [byte]
            (format "%02x" (bit-and 0xff byte)))]
    (->> bytes (map to-hex) (apply str))))

(defn zip-str
  "Zip the given data with zlib. Return a ByteArrayInputStream of the zipped
  content."
  [data]
  (let [out (ByteArrayOutputStream.)
        zipper (DeflaterOutputStream. out)]
    (io/copy data zipper)
    (.close zipper)
    (ByteArrayInputStream. (.toByteArray out))))

(defn write-tree [addresses name root-dir db print-check]
  (let [tree-contents (byte-array (concat (.getBytes (str "tree " (reduce + 0 (map count addresses)) "\000")) (apply concat addresses)))
        tree-adr (sha-bytes tree-contents)
        root-path (str root-dir "/" db "/objects/")
        hex-adr (to-hex-string tree-adr)
        path (str root-path (subs hex-adr 0 2) "/" (subs hex-adr 2))
        entry-bytes (.getBytes (str "40000 " name "\000"))]
    (cond
      (not (.exists (io/as-file (str root-path (subs hex-adr 0 2))))) (do
                                                                        (.mkdir (File. (str root-path (subs hex-adr 0 2))))
                                                                        (io/copy (zip-str tree-contents) (io/file path))
                                                                        (cond
                                                                          (= print-check "true") (println hex-adr)
                                                                          :else (concat entry-bytes tree-adr)))
      (= print-check "true") (println hex-adr)
      :else (concat entry-bytes tree-adr))))

(defn create-tree [db, curr-path, root-dir]
  (let [files (.listFiles (io/file curr-path))]
    (doall (for [f (sort files) :when (and (not= (.getName f) db) (not (and (.isDirectory f) (= (count (.listFiles f)) 0))))]
             (cond
               (.isDirectory f) (cond
                                  (not= (count (.listFiles f)) 0) (let [addresses (create-tree db f root-dir)]
                                                                    (write-tree (vec addresses) (.getName f) root-dir db "false")))
               (.isFile f) (let [header+blob (str "blob " (count (slurp f)) "\000" (slurp f))
                                 bytes (sha-bytes (.getBytes header+blob))
                                 path (str root-dir "/" db "/objects/")
                                 hex-bytes (to-hex-string bytes)]
                             (cond
                               (not (.exists (io/as-file (str path (subs hex-bytes 0 2) "/" (subs hex-bytes 2)))))
                               (do
                                 (.mkdir (File. (str path (subs hex-bytes 0 2))))
                                 (io/copy (zip-str header+blob) (io/file (str path (subs hex-bytes 0 2) "/" (subs hex-bytes 2))))
                                 (concat (.getBytes (str "100644 " (.getName f) "\000")) bytes))
                               :else (concat (.getBytes (str "100644 " (.getName f) "\000")) bytes))))))))

(defn write-wtree [x, dir, db]
  (cond
    (or (= (first x) "--help") (= (first x) "-h")) (println "idiot write-wtree: write the working tree to the database\n\nUsage: idiot write-wtree\n\nArguments:\n   -h       print this message")
    (not= (count x) 0) (println "Error: write-wtree accepts no arguments")
    (not (.exists (io/as-file (str dir "/" db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
    (= (count (.listFiles (io/as-file (str dir)))) 1) (println "The directory was empty, so nothing was saved.")
    :else (let [addresses (create-tree db dir dir)]
            (cond
              (= (seq addresses) nil) (println "The directory was empty, so nothing was saved.")
              :else (write-tree addresses dir dir db "true")))))
