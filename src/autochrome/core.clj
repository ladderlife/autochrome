(ns autochrome.core
  (:require [autochrome.page :as page]
            [clojure.tools.cli :as cli])
  (:import [java.awt Desktop]
           [java.io File])
  (:gen-class))

(def cli-options
  [["-o" "--open" "If set, write HTML to a temp file and try to open it in a browser"]])

(defn -main
  [& args]
  (let [{:keys [options arguments]} (cli/parse-opts args cli-options)
        [a b c] arguments
        the-page (cond
                   c (page/pull-request-diff a b (Integer/parseInt c))
                   b (page/local-diff a b))]
    (cond
      (nil? the-page)
      (println "expected 2 or 3 args [treeA treeB] or [owner repo pr-id] ")

      (and (:open options) (Desktop/isDesktopSupported))
      (.browse (Desktop/getDesktop)
               (.toURI
                (doto (File/createTempFile "diff" ".html")
                  (spit the-page))))
      :else (println the-page)))
  (shutdown-agents))
