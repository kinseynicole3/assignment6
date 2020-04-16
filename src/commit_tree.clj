(ns commit-tree
  (:require [clojure.java.io :as io]
            [cat-file]
            [write-wtree]
            [hash-object]
            [clojure.string :as string])
  (:import (java.io File)))

(defn commit-object [tree-adr, message, parents]
  (let [author-str "Linus Torvalds <torvalds@transmeta.com> 1581997446 -0500"
        commit-format (str "tree %s\n"
                           "%s"
                           "author %s\n"
                           "committer %s\n"
                           "\n"
                           "%s\n")
        commit-str (format commit-format
                           tree-adr
                           parents
                           author-str
                           author-str
                           message)]
    (format "commit %d\000%s"
            (count commit-str)
            commit-str)))

(defn get-address [args, dir, db]
  (cond
    (= (count args) 40) (str args)
    :else (let [length (count (subs args 2))
                addy (loop [i 0]
                       (when (< i (count (.list (io/file (str dir "/" db "/objects/" (subs args 0 2))))))
                         (let [dir (.list (io/file (str dir "/" db "/objects/" (subs args 0 2))))]
                           (if (= (subs (get dir i) 0 length) (subs args 2))
                             (str (subs args 0 2) (get dir i)) (recur (inc i))))))]
            ;(println (subs args 2))
            (cond
              (= addy nil) args
              :else addy))))

(defn get-parent-addresses [parents, dir, db]
  (doall (for [parent parents]
           (cond
             (= (count parent) 40) (str parent)
             (< (count parent) 4) (str parent)
             :else (get-address parent dir db)))))

(defn skip-nil [parents-vec]
  (loop [i 0]
    (when (< i (count parents-vec))
      (if (not= (nth parents-vec i) nil) (nth parents-vec i) (recur (inc i))))))

(defn parent-length-check [parents-vec]
  (doall (for [parent parents-vec]
           (cond
             (< (count parent) 4) parent))))

(defn parents-check [root-path parents-vec]
  (doall (for [parent parents-vec]
           (cond
             (not (.exists (io/as-file (str root-path (subs parent 0 2) "/" (subs parent 2))))) parent))))

(defn parents-type-check
  "Returns a list of parents with valid addresses that aren't commit objects"
  [root-path parents-vec]
  (doall (for [parent parents-vec]
           (cond
             (= parent nil) parent
             (.exists (io/as-file (str root-path (subs parent 0 2) "/" (subs parent 2))))
             (let [type (cat-file/check-type (io/as-file (str root-path (subs parent 0 2) "/" (subs parent 2))))]
               (cond (not= type "commit") parent))))))

(defn ambiguous-address-check [args, dir, db]
  (let [addresses (.list (io/file (str dir "/" db "/objects/" (subs args 0 2))))
        length (count (subs args 2))
        file-address (subs args 2)]
    (doall (for [address addresses]
             (cond
               (= (subs address 0 length) file-address) address)))))

(defn parent-ambiguous-check [parents, dir, db]
  (doall (for [parent parents]
           (cond
             (> (count (ambiguous-address-check parent, dir, db)) 1) (str parent)))))

(defn commit-tree [x, dir, db]
  (cond
    (or (= (first x) "--help") (= (first x) "-h")) (println "idiot commit-tree: write a commit object based on the given tree\n\nUsage: idiot commit-tree <tree> -m \"message\" [(-p parent)...]\n\nArguments:\n   -h               print this message\n   <tree>           the address of the tree object to commit\n   -m \"<message>\"   the commit message\n   -p <parent>      the address of a parent commit")
    (= (.exists (io/as-file (str dir "/" db "/objects/"))) false) (println "Error: could not find database. (Did you run `idiot init`?)")
    (or (= (count x) 0) (= (first x) "-m")) (println "Error: you must specify a tree address.")
    (< (count (first x)) 4) (println (str "Error: too few characters specified for address '" (first x) "'"))
    (not= (count (get-address (first x) dir db)) 40) (println "Error: no tree object exists at that address.")
    (not (.exists (io/as-file (str dir "/" db "/objects/" (subs (first x) 0 2) "/" (subs (get-address (first x) dir db) 2))))) (println "Error: no tree object exists at that address.")
    (or (and (= (second x) "-m") (= (count (rest x)) 1)) (and (= (second x) "-m") (= (first (drop 2 x)) "-p"))) (println "Error: you must specify a message with the -m switch.")
    (= (last x) "-p") (println "Error: you must specify a commit object with the -p switch.")
    (= (count (rest x)) 1) (println "Error: you must specify a message with the -m switch.")
    (= (first (drop 3 x)) "-p") (let [message (first (drop 2 x))
                                      tree-adr (get-address (first x) dir db)
                                      parent-ambiguous (skip-nil (parent-ambiguous-check (remove #(= % "-p") (rest (rest (rest x)))) dir db))
                                      parents (get-parent-addresses (remove #(= % "-p") (rest (rest (rest x)))) dir db)
                                      path (str dir "/" db "/objects/")
                                      length-check (skip-nil (parent-length-check parents))
                                      validity (skip-nil (parents-check path parents))
                                      type-check (skip-nil (parents-type-check path parents))
                                      address-string (string/join "\n" (for [address parents] (str "parent " address)))]
                                  (cond
                                    (not= parent-ambiguous nil) (println (str "Error: ambiguous match for address '" parent-ambiguous "'"))
                                    (not= length-check nil) (println (str "Error: too few characters specified for address '" length-check "'"))
                                    (not= validity nil) (println (str "Error: no commit object exists at address " validity "."))
                                    (not= type-check nil) (println (str "Error: an object exists at address " type-check ", but it isn't a commit."))
                                    (not= (cat-file/check-type (str dir "/" db "/objects/" (subs (get-address (first x) dir db) 0 2) "/" (subs (get-address (first x) dir db) 2))) "tree") (println "Error: an object exists at that address, but it isn't a tree.")
                                    (not= (second x) "-m") (println "Error: you must specify a message.")
                                    :else (let [commit-obj (commit-object tree-adr message (str address-string "\n"))
                                                commit-adr (write-wtree/to-hex-string (write-wtree/sha-bytes (.getBytes commit-obj)))
                                                path (str dir "/" db "/objects/")]
                                            (cond
                                              (not (.exists (io/as-file (str path (subs commit-adr 0 2) "/" (subs commit-adr 2)))))
                                              (do
                                                (.mkdir (File. (str path (subs commit-adr 0 2))))
                                                (io/copy (hash-object/zip-str commit-obj) (io/file (str path (subs commit-adr 0 2) "/" (subs commit-adr 2))))))
                                            (println commit-adr))))
    :else (let [message (first (drop 2 x))
                tree-adr (get-address (first x) dir db)
                commit-obj (commit-object tree-adr message "")
                commit-adr (write-wtree/to-hex-string (write-wtree/sha-bytes (.getBytes commit-obj)))
                path (str dir "/" db "/objects/")
                matching-addresses (ambiguous-address-check (first x) dir db)]
            (cond
              (not (.exists (io/as-file (str path (subs commit-adr 0 2) "/" (subs commit-adr 2)))))
              (do
                (.mkdir (File. (str path (subs commit-adr 0 2))))
                (io/copy (hash-object/zip-str commit-obj) (io/file (str path (subs commit-adr 0 2) "/" (subs commit-adr 2))))))

            (cond
              (not= (count matching-addresses) 1) (println (str "Error: ambiguous match for address '" (first x) "'"))
              (not= (cat-file/check-type (str dir "/" db "/objects/" (subs (get-address (first x) dir db) 0 2) "/" (subs (get-address (first x) dir db) 2))) "tree") (println "Error: an object exists at that address, but it isn't a tree.")
              (not= (second x) "-m") (println "Error: you must specify a message.")
              :else (println commit-adr)))))

