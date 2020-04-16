(ns switch
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn switch [x, dir, db]
  (cond
    (or (= (first x) "-h") (= (first x) "--help")) (println "idiot switch: change what HEAD points to\n\nUsage: idiot switch [-c] <branch>\n\nArguments:\n   -c   create the branch before switching to it")
    (= (count x) 0) (println "Error: you must specify a branch name.")
    (and (> (count x) 2) (= (first x) "-c")) (println "Error: you may only specify one branch name.")
    (and (> (count x) 1) (not= (first x) "-c")) (println "Error: you may only specify one branch name.")
    (not (.exists (io/as-file (str dir "/" db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
    (and (= (first x) "-c") (.exists (io/as-file (str dir "/" db "/refs/heads/" (second x))))) (println "Error: a ref with that name already exists.")
    (and (not= (first x) "-c") (not (.exists (io/as-file (str dir "/" db "/refs/heads/" (first x)))))) (println "Error: no ref with that name exists.")
    (not= (first x) "-c") (do
                            (spit (str dir "/" db "/HEAD") (str "ref: refs/heads/" (first x) "\n"))
                            (print (str "Switched to branch '" (first x) "'\n")))
    :else (let [root-path (str dir "/" db)
                head-contents (slurp (str dir "/" db "/HEAD"))
                split-contents (str/split head-contents #" ")
                commit-address (slurp (str root-path "/" (get (str/split (get split-contents 1) #"\n") 0)))
                path (str dir "/" db "/refs/heads/" (second x))]
            (spit path commit-address)
            (spit (str dir "/" db "/HEAD") (str "ref: refs/heads/" (second x) "\n"))
            (print (str "Switched to a new branch '" (second x) "'\n")))))
