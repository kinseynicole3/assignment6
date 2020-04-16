(ns branch
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn branch [x, dir, db]
  (cond
    (or (= (first x) "-h") (= (first x) "--help")) (println "idiot branch: list or delete branches\n\nUsage: idiot branch [-d <branch>]\n\nArguments:\n   -d <branch>   delete branch <branch>")
    (and (= (count x) 1) (= (first x) "-d")) (println "Error: you must specify a branch name.")
    (and (= (count x) 1) (not= (first x) "-d")) (println "Error: invalid arguments.")
    (and (not= (first x) "-d") (> (count x) 1)) (println "Error: invalid arguments.")
    (and (= (count x) 2) (not= (first x) "-d")) (println "Error: invalid arguments.")
    (> (count x) 2) (println "Error: invalid arguments.")
    (not (.exists (io/as-file (str dir "/" db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
    (and (= (first x) "-d") (not (.exists (io/as-file (str dir "/" db "/refs/heads/" (second x)))))) (println (str "Error: branch '" (second x) "' not found."))
    (= (first x) "-d") (let [head-contents (slurp (str dir "/" db "/HEAD"))
                             split-contents (get (str/split (get (str/split head-contents #" ") 1) #"\n") 0)
                             path (str "refs/heads/" (second x))]
                         (cond
                           (= split-contents path) (println (str "Error: cannot delete checked-out branch '" (second x) "'."))
                           :else (do
                                   (io/delete-file (str dir "/" db "/" path))
                                   (println (str "Deleted branch " (second x) ".")))))
    :else (let [head-contents (slurp (str dir "/" db "/HEAD"))
                files (.list (io/file (str dir "/" db "/refs/heads")))]
            (doall (for [f (sort files)]
                     (cond
                       (= (get (str/split (last (str/split (str/trim-newline head-contents) #"/")) #"\n") 0) f) (println "*" f)
                       :else (println " " f)))))))