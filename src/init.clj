(ns init
  (:require [clojure.java.io :as io])
  (:import (java.io File)))

(defn create-directory [dir db]
  (.mkdir (File. (str dir "/" db)))
  (.mkdir (File. (str dir "/" db "/objects/")))
  (.mkdirs (File. (str dir "/" db "/refs/heads")))
  (spit (str dir "/" db "/HEAD") "ref: refs/heads/master\n")
  (println "Initialized empty Idiot repository in .idiot directory"))

(defn init [x, dir, db]
  (cond
    (> (count x) 0) (cond
                      (or (= (first x) "-h") (= (first x) "--help")) (println "idiot init: initialize a new database\n\nUsage: idiot init\n\nArguments:\n   -h   print this message")
                      :else (println "Error: init accepts no arguments"))
    (= (.exists (io/as-file (str dir "/" db))) true) (println "Error: .idiot directory already exists")
    (= (.exists (io/as-file (str dir "/" db))) false) (create-directory dir db)))

