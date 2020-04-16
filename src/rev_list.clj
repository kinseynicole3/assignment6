(ns rev-list
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io ByteArrayOutputStream)
           (java.util.zip InflaterInputStream)))

;; x is a vector containing [<ref>] or [-n num <ref>]

(defn unzip
  "Unzip the given file's contents with zlib."
  [path]
  (with-open [input (-> path io/file io/input-stream)
              unzipper (InflaterInputStream. input)
              out (ByteArrayOutputStream.)]
    (io/copy unzipper out)
    (.toByteArray out)))

;; TODO: change this function so it returns ref if not '@' or 'HEAD'. else: find head ref and return that.
(defn get-head-contents [dir-db ref]
  (if (and (not (nil? ref)) (not= ref "HEAD") (not= ref "@"))
    ref
    (let [head-contents (slurp (io/as-file (str dir-db "/HEAD")))
          head-list (str/split head-contents #"/")]
      (if (.contains head-contents "ref") ;; checks if HEAD contains ref or commit
        (str/trim-newline (last head-list)) head-contents))))

(defn recurse [dir-db commit-address]
  (let [commit-object (unzip (io/as-file (str dir-db "/objects/" (subs commit-address 0 2) "/" (subs commit-address 2))))
        contents (get (str/split (slurp commit-object) #"\000") 1)
        contents-list (str/split (str/replace contents #"\n" " ") #" ")]
    (if (= (nth contents-list 2) "parent")
      (do
        (println commit-address)
        (recurse dir-db (nth contents-list 3)))
      (println commit-address))))

(defn recursive-print [dir-db ref]
  (if (not (.exists (io/as-file (str dir-db "/refs/heads/" ref))))
    (println (str "Error: could not find ref named " ref "."))
    (let [commit-address (str/trim-newline (slurp (io/as-file (str dir-db "/refs/heads/" ref))))
          commit-object (unzip (io/as-file (str dir-db "/objects/" (subs commit-address 0 2) "/" (subs commit-address 2))))
          contents (get (str/split (slurp commit-object) #"\000") 1)
          contents-list (str/split (str/replace contents #"\n" " ") #" ")]
      (if (= (nth contents-list 2) "parent")
        (do
          (println commit-address)
          (recurse dir-db (nth contents-list 3)))
        (println commit-address)))))

;; TODO: check to make sure ref exists. if not, print a statement
(defn fixed-print [dir-db ref n]
  (if (not (.exists (io/as-file (str dir-db "/refs/heads/" ref))))
    (println (str "Error: could not find ref named " ref "."))
    (let [commit-address (str/trim-newline (slurp (io/as-file (str dir-db "/refs/heads/" ref))))
          commit-object (unzip (str dir-db "/objects/" (subs commit-address 0 2) "/" (subs commit-address 2)))
          contents (get (str/split (slurp commit-object) #"\000") 1)
          contents-list (str/split (str/replace contents #"\n" " ") #" ")]
      (println commit-address)
      (loop [x n
             contents0 contents-list]
        (cond
          (= x 0) nil
          (not= (nth contents0 2) "parent") nil
          :else (do
                  (println (nth contents0 3))
                  (let [commit-object (unzip (str dir-db "/objects/" (subs (nth contents0 3) 0 2) "/" (subs (nth contents0 3) 2)))
                        contents-new (get (str/split (slurp commit-object) #"\000") 1)
                        contents-new-list (str/split (str/replace contents-new #"\n" " ") #" ")]
                    (recur (dec x) contents-new-list))))))))

(defn n-flag? [pars]
  (if (and (> (count pars) 1) (= (first pars) "-n")) true false))

(defn rev-list [x, dir, db]
  (try
    (cond
      (or (= (first x) "-h") (= (first x) "--help")) (println "idiot rev-list: list preceding revisions, latest first\n\nUsage: idiot rev-list [-n <count>] [<ref>]\n\nArguments:\n   -n <count>   stop after <count> revisions (default: don't stop)\n   <ref>        a reference; see the rev-parse command (default: HEAD)")
      (= (.exists (io/as-file (str dir "/" db "/objects/"))) false) (println "Error: could not find database. (Did you run `idiot init`?)")
      (and (= (count x) 1) (= (first x) "-n")) (println "Error: you must specify a numeric count with '-n'.")
      (and (> (count x) 1) (not (number? (Integer/parseInt (second x))))) (println "Error: you must specify a numeric count with '-n'.")
      (and (= (first x) "-n") (< (Integer/parseInt (second x)) 0)) (println "Error: the argument for '-n' must be a non-negative integer.")
      :else (if (n-flag? x)
              (if (= (count x) 2)
                (fixed-print (str dir "/" db) (get-head-contents (str dir "/" db) "HEAD") (Integer/parseInt (nth x 1)))
                (fixed-print (str dir "/" db) (get-head-contents (str dir "/" db) (nth x 2)) (Integer/parseInt (nth x 1))))
              (recursive-print (str dir "/" db) (get-head-contents (str dir "/" db) (first x)))))
    (catch Exception _e
      (println "Error: the argument for '-n' must be a non-negative integer."))))

