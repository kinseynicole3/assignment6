(ns cat-file
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [conversions :as con])
  (:import (java.util.zip InflaterInputStream)
           (java.io ByteArrayOutputStream)))

(defn ambiguous-check [args, dir, db]
  (let [addresses (.list (io/file (str dir "/" db "/objects/" (subs args 0 2))))
        length (count (subs args 2))
        file-address (subs args 2)]
    (doall (for [address addresses]
             (cond
               (= (subs address 0 length) file-address) address)))))

(defn check-type [path]
  (get (str/split (slurp (con/unzip path)) #" ") 0))

(defn unzip
  "Unzip the given data with zlib. Pass an opened input stream as the arg. The
  caller should close the stream afterwards."
  [input-stream]
  (with-open [unzipper (InflaterInputStream. input-stream)
              out (ByteArrayOutputStream.)]
    (io/copy unzipper out)
    (->> (.toByteArray out)
         (map char)
         (apply str))))

(defn get-address [args, dir, db]
  (loop [i 0]
    (when (< i (count (.list (io/file (str dir "/" db "/objects/")))))
      (if (= (get (.list (io/file (str dir "/" db "/objects/"))) i) (subs args 0 2)) (let [address-list (.list (io/file (str dir "/" db "/objects/" (subs args 0 2))))]
                                                                                       (str (subs args 0 2) (first address-list))) (recur (inc i))))))

(defn cat-file [x, dir, db]
  (cond
    (or (= (first x) "-h") (= (first x) "--help")) (println "idiot cat-file: print information about an object\n\nUsage: idiot cat-file {-p|-t} <address>\n\nArguments:\n   -h          print this message\n   -p          pretty-print contents based on object type\n   -t          print the type of the given object\n   <address>   the SHA1-based address of the object")
    (= (.exists (io/as-file (str dir "/" db "/objects/"))) false) (println "Error: could not find database. (Did you run `idiot init`?)")
    (and (not= (first x) "-p") (not= (first x) "-t")) (println "Error: the -p or -t switch is required")
    (= (count x) 1) (println "Error: you must specify an address")
    (< (count (second x)) 4) (println (str "Error: too few characters specified for address '" (second x) "'"))
    (> (count (ambiguous-check (second x) dir db)) 1) (println (str "Error: ambiguous match for address '" (second x) "'"))
    (= (first x) "-p") (cond
                         (= (count (second x)) 40) (let [path (str dir "/" db "/objects/" (subs (first (rest x)) 0 2) "/" (subs (first (rest x)) 2))]
                                                     (cond
                                                       (= (.exists (io/as-file path)) false) (println "Error: that address doesn't exist")
                                                       (not= (check-type path) "tree") (print (get (str/split (with-open [input (-> path io/file io/input-stream)] (unzip input)) #"\000") 1))
                                                       :else (println "idk how to do this")))
                         :else (let [address (get-address (second x) dir db)]
                                 (cond
                                   (= address (subs (second x) 0 2)) (println (str "Error: ambiguous match for address '" (second x) "'"))
                                   (= (count address) 40) (let [path (str dir "/" db "/objects/" (subs address 0 2) "/" (subs address 2))]
                                                            (cond
                                                              (= (.exists (io/as-file path)) false) (println "Error: that address doesn't exist")
                                                              (not= (check-type path) "tree") (print (get (str/split (with-open [input (-> path io/file io/input-stream)] (unzip input)) #"\000") 1))
                                                              :else (println "idk how to do this"))))))
    (= (first x) "-t") (cond
                         (= (count (second x)) 40) (let [path (str dir "/" db "/objects/" (subs (first (rest x)) 0 2) "/" (subs (first (rest x)) 2))]
                                                     (cond
                                                       (= (.exists (io/as-file path)) false) (println "Error: that address doesn't exist")
                                                       :else (println (check-type path))))
                         :else (let [address (get-address (second x) dir db)]
                                 (cond
                                   (= address (subs (second x) 0 2)) (println (str "Error: ambiguous match for address '" (second x) "'"))
                                   (= (count address) 40) (let [path (str dir "/" db "/objects/" (subs address 0 2) "/" (subs address 2))]
                                                            (cond
                                                              (= (.exists (io/as-file path)) false) (println "Error: that address doesn't exist")
                                                              :else (println (check-type path)))))))
    :else (println "Error: you must specify an address")))