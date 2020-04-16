(ns explore
  (:require  [clojure.java.io :as io]
             [ring.adapter.jetty :refer [run-jetty]]
             [hiccup.page :refer [html5]]))

(defn list-branches [dir db]
  (let [files (.list (io/file (str dir "/" db "/refs/heads")))]
    (doall (for [f (sort files)]
             [:li f]))))

(defn format-html-body [dir db]
  (let [branches (list-branches dir db)
        formatted-branches (conj [:body] (concat branches))]
    (html5 [:head [:title "Branches"]] formatted-branches)))

(defn handle [dir db port]
  (let [body (format-html-body dir db)
        handler (fn [_request]
                  {:status  200
                   :headers {"Content-Type" "text/html"}
                   :body  body})]
    (run-jetty handler {:port port})))

(defn start-server [dir db port]
  (println (str "Starting server on port " port "."))
  (handle dir db port))

(defn get-port [x]
  (cond
    (empty? x) 3000
    (and (> (count x) 1) (= (first x) "-p")) (Integer/parseInt (second x))))

(defn explore [x, dir, db]
  (try
    (cond
      (or (= (first x) "-h") (= (first x) "--help")) (println "idiot explore: start a web server to explore the database\n\nUsage: idiot explore [-p <port>]\n\nArguments:\n   -p <port>   listen on the given port (default: 3000)")
      (= (.exists (io/as-file (str dir "/" db "/objects/"))) false) (println "Error: could not find database. (Did you run `idiot init`?)")
      (and (= (count x) 1) (= (first x) "-p")) (println "Error: you must specify a numeric port with '-p'.")
      (and (= (first x) "-n") (< (Integer/parseInt (second x)) 0)) (println "Error: the argument for '-n' must be a non-negative integer.")
      :else (let [port (get-port x)]
              (start-server dir db port)))
    (catch Exception _e
      (println "Error: the argument for '-p' must be a non-negative integer."))))
