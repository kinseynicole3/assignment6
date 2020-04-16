(ns idiot
  (:require [clojure.java.io :as io]
            [hash-object]
            [help]
            [write-wtree]
            [commit-tree]
            [init]
            [cat-file]
            [rev-parse]
            [switch]
            [branch]
            [commit]
            [rev-list]
            [log]
            [explore]))

(defn switches [args db dir]
  (let [command-check (fn [command]
                        (command (rest args) dir db))
        d-switch (fn [args]
                   (cond
                     (< (count args) 2) (println "Error: the -d switch needs an argument")
                     :else (switches (rest args) (first args) dir)))
        r-switch (fn [args]
                   (cond
                     (< (count args) 2) (println "Error: the -r switch needs an argument")
                     (= (.exists (io/as-file (first args))) false) (println "Error: the directory specified by -r does not exist")
                     :else (switches (rest args) db (str (first args)))))]
    (cond
      (= (count args) 0) (println "idiot: the other stupid content tracker\n\nUsage: idiot [<top-args>] <command> [<args>]\n\nTop-level arguments:\n   -r <dir>   run from the given directory instead of the current one\n   -d <dir>   store the database in <dir> (default: .idiot)\n\nCommands:\n   branch [-d <branch>]\n   cat-file {-p|-t} <address>\n   commit <tree> -m \"message\" [(-p parent)...]\n   commit-tree <tree> -m \"message\" [(-p parent)...]\n   explore [-p <port>]\n   hash-object [-w] <file>\n   help\n   init\n   log --oneline [-n <count>] [<ref>]\n   rev-list [-n <count>] [<ref>]\n   rev-parse <ref>\n   switch [-c] <branch>\n   write-wtree")
      (= (first args) "help") (help/help (rest args))
      (= (first args) "-r") (r-switch (rest args))
      (= (first args) "-d") (d-switch (rest args))
      (or (= (first args) "-h") (= (first args) "--help")) (println "idiot: the other stupid content tracker\n\nUsage: idiot [<top-args>] <command> [<args>]\n\nTop-level arguments:\n   -r <dir>   run from the given directory instead of the current one\n   -d <dir>   store the database in <dir> (default: .idiot)\n\nCommands:\n   branch [-d <branch>]\n   cat-file {-p|-t} <address>\n   commit <tree> -m \"message\" [(-p parent)...]\n   commit-tree <tree> -m \"message\" [(-p parent)...]\n   explore [-p <port>]\n   hash-object [-w] <file>\n   help\n   init\n   log --oneline [-n <count>] [<ref>]\n   rev-list [-n <count>] [<ref>]\n   rev-parse <ref>\n   switch [-c] <branch>\n   write-wtree")
      (= (first args) "init") (command-check init/init)
      (= (first args) "hash-object") (command-check hash-object/hash-object)
      (= (first args) "cat-file") (command-check cat-file/cat-file)
      (= (first args) "write-wtree") (command-check write-wtree/write-wtree)
      (= (first args) "commit-tree") (command-check commit-tree/commit-tree)
      (= (first args) "rev-parse") (command-check rev-parse/rev-parse)
      (= (first args) "switch") (command-check switch/switch)
      (= (first args) "branch") (command-check branch/branch)
      (= (first args) "commit") (command-check commit/commit)
      (= (first args) "rev-list") (command-check rev-list/rev-list)
      (= (first args) "log") (command-check log/log)
      (= (first args) "explore") (command-check explore/explore)
      :else (println "Error: invalid command"))))

(defn -main [& args]
  (switches args ".idiot" "./"))