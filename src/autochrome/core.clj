(ns autochrome.core
  (:require [autochrome.diff :as diff]
            [autochrome.github :as github]
            [autochrome.page :as page]
            [clojure.java.io :as io]
            [clojure.tools.cli :as cli])
  (:import [java.awt Desktop]
           [java.io File])
  (:gen-class))

(def cli-options
  [[nil "--open" "If set, write HTML to a temp file and try to open it in a browser"]
   ["-t" "--token TOKEN" "github api bearer auth token e.g. username:123abcdef"]
   ["-o" "--output FILE" "output filename"]
   [nil "--clojure-only" "only show clojure diffs"]
   [nil "--git-dir PATH" "path to the git repo"]])

(defn do-main
  [& args]
  (let [{:keys [options arguments]} (cli/parse-opts args cli-options)
        [a b c] arguments
        the-page (binding [github/*auth-token* (:token options)
                           page/*clojure-only* (:clojure-only options)
                           github/*git-dir* (or (:git-dir options) ".")]
                   (cond
                     c (page/pull-request-diff a b (Integer/parseInt c))
                     b (page/local-diff a b)
                     a (page/local-diff-work-tree a)))
        output-file (if (:output options)
                      (io/file (:output options))
                      (File/createTempFile "diff" ".html"))]
    (binding [*out* *err*] (println 'processed @diff/nprocessed 'states))
    (if-not the-page
      (println "expected 2 or 3 args [treeA treeB] or [owner repo pr-id] ")
      (spit output-file the-page))
    (if (and (:open options) (Desktop/isDesktopSupported))
      (.browse (Desktop/getDesktop)
               (.toURI output-file))
      (when-not (:output options)
        (io/copy output-file *out*)))))

(defn -main
  [& args]
  (apply do-main args)
  (shutdown-agents))
