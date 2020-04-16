(ns rev-parse
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn rev-parse [x, dir, db]
  (cond
    (or (= (first x) "-h") (= (first x) "--help")) (println "idiot rev-parse: determine which commit a ref points to\n\nUsage: idiot rev-parse <ref>\n\n<ref> can be:\n- a branch name, like 'master'\n- literally 'HEAD'\n- literally '@', an alias for 'HEAD'")
    (= (count x) 0) (println "Error: you must specify a branch name.")
    (> (count x) 1) (println "Error: you must specify a branch name and nothing else.")
    (not (.exists (io/as-file (str dir "/" db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
    (or (= (first x) "HEAD") (= (first x) "@")) (let [root-path (str dir "/" db)
                                                      head-contents (slurp (str dir "/" db "/HEAD"))
                                                      split-contents (str/split head-contents #" ")]
                                                  (cond
                                                    (= (get split-contents 0) "ref:") (print (slurp (str root-path "/" (get (str/split (get split-contents 1) #"\n") 0))))
                                                    :else (print head-contents)))
    (not (.exists (io/as-file (str dir "/" db "/refs/heads/" (first x))))) (println (str "Error: could not find ref named " (first x) "."))
    :else (print (slurp (str dir "/" db "/refs/heads/" (first x))))))

