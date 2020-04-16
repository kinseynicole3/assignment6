(ns commit
  (:require [clojure.java.io :as io]
            [cat-file]
            [clojure.string :as str]
            [commit-tree :as ct]
            [hash-object]
            [write-wtree])
  (:import (java.io File)))

(defn update-head-contents [commit-adr, dir, db]
  (let [head-contents (slurp (str dir "/" db "/HEAD"))
        split-contents (str/trim (get (str/split head-contents #" ") 1))
        name (get (str/split (last (str/split (str/trim-newline head-contents) #"/")) #"\n") 0)
        ref-adr (str dir "/" db "/" split-contents)]
    (cond
      (.exists (io/as-file ref-adr)) (do
                                       (spit ref-adr (str commit-adr "\n"))
                                       (println "Commit created.")
                                       (println (str "Updated branch " name ".")))
      :else (do
              (spit ref-adr (str commit-adr "\n"))
              (println (str "Updated branch " name "."))
              (println "Commit created.")))))

(defn commit [x, dir, db]
  (cond
    (or (= (first x) "-h") (= (first x) "--help")) (println "idiot commit: create a commit and advance the current branch\n\nUsage: idiot commit <tree> -m \"message\" [(-p parent)...]\n\nArguments:\n   -h               print this message\n   <tree>           the address of the tree object to commit\n   -m \"<message>\"   the commit message\n   -p <parent>      the address of a parent commit")
    (= (.exists (io/as-file (str dir "/" db "/objects/"))) false) (println "Error: could not find database. (Did you run `idiot init`?)")
    (or (= (count x) 0) (= (first x) "-m")) (println "Error: you must specify a tree address.")
    (< (count (first x)) 4) (println (str "Error: too few characters specified for address '" (first x) "'"))
    (not= (count (ct/get-address (first x) dir db)) 40) (println "Error: no tree object exists at that address.")
    (not (.exists (io/as-file (str dir "/" db "/objects/" (subs (first x) 0 2) "/" (subs (ct/get-address (first x) dir db) 2))))) (println "Error: no tree object exists at that address.")
    (or (and (= (second x) "-m") (= (count (rest x)) 1)) (and (= (second x) "-m") (= (first (drop 2 x)) "-p"))) (println "Error: you must specify a message with the -m switch.")
    (= (last x) "-p") (println "Error: you must specify a commit object with the -p switch.")
    (= (count (rest x)) 1) (println "Error: you must specify a message with the -m switch.")
    (= (first (drop 3 x)) "-p") (let [message (first (drop 2 x))
                                      tree-adr (ct/get-address (first x) dir db)
                                      parent-ambiguous (ct/skip-nil (ct/parent-ambiguous-check (remove #(= % "-p") (rest (rest (rest x)))) dir db))
                                      parents (ct/get-parent-addresses (remove #(= % "-p") (rest (rest (rest x)))) dir db)
                                      path (str dir "/" db "/objects/")
                                      validity (ct/skip-nil (ct/parents-check path parents))
                                      length-check (ct/skip-nil (ct/parent-length-check parents))
                                      type-check (ct/skip-nil (ct/parents-type-check path parents))
                                      address-string (str/join "\n" (for [address parents] (str "parent " address)))]
                                  (cond
                                    (not= parent-ambiguous nil) (println (str "Error: ambiguous match for address '" parent-ambiguous "'"))
                                    (not= length-check nil) (println (str "Error: too few characters specified for address '" length-check "'"))
                                    (not= validity nil) (println (str "Error: no commit object exists at address " validity "."))
                                    (not= type-check nil) (println (str "Error: an object exists at address " type-check ", but it isn't a commit."))
                                    (not= (cat-file/check-type (str dir "/" db "/objects/" (subs (ct/get-address (first x) dir db) 0 2) "/" (subs (ct/get-address (first x) dir db) 2))) "tree") (println "Error: an object exists at that address, but it isn't a tree.")
                                    (not= (second x) "-m") (println "Error: you must specify a message.")
                                    :else (let [commit-obj (ct/commit-object tree-adr message (str address-string "\n"))
                                                commit-adr (write-wtree/to-hex-string (write-wtree/sha-bytes (.getBytes commit-obj)))
                                                path (str dir "/" db "/objects/")]
                                            (cond
                                              (not (.exists (io/as-file (str path (subs commit-adr 0 2) "/" (subs commit-adr 2)))))
                                              (do
                                                (.mkdir (File. (str path (subs commit-adr 0 2))))
                                                (io/copy (hash-object/zip-str commit-obj) (io/file (str path (subs commit-adr 0 2) "/" (subs commit-adr 2))))))
                                            (cond
                                              (not= (count (slurp (str dir "/" db "/HEAD"))) 41) (update-head-contents commit-adr dir db)
                                              :else (println "Commit created.")))))
    :else (let [message (first (drop 2 x))
                tree-adr (ct/get-address (first x) dir db)
                commit-obj (ct/commit-object tree-adr message "")
                commit-adr (write-wtree/to-hex-string (write-wtree/sha-bytes (.getBytes commit-obj)))
                path (str dir "/" db "/objects/")
                matching-addresses (ct/ambiguous-address-check (first x) dir db)]
            (cond
              (not (.exists (io/as-file (str path (subs commit-adr 0 2) "/" (subs commit-adr 2)))))
              (do
                (.mkdir (File. (str path (subs commit-adr 0 2))))
                (io/copy (hash-object/zip-str commit-obj) (io/file (str path (subs commit-adr 0 2) "/" (subs commit-adr 2))))))

            (cond
              (not= (count matching-addresses) 1) (println (str "Error: ambiguous match for address '" (first x) "'"))
              (not= (cat-file/check-type (str dir "/" db "/objects/" (subs (ct/get-address (first x) dir db) 0 2) "/" (subs (ct/get-address (first x) dir db) 2))) "tree") (println "Error: an object exists at that address, but it isn't a tree.")
              (not= (second x) "-m") (println "Error: you must specify a message.")
              (not= (count (slurp (str dir "/" db "/HEAD"))) 41) (update-head-contents commit-adr dir db)
              :else (println "Commit created.")))))
