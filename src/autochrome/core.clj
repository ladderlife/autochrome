(ns autochrome.core
  (:require [autochrome.page :as page]
            [clj-http.client :as http])
  
  (:gen-class))

(defn -main
  [& args]
  #_(println
    (http/get
      "https://raw.githubusercontent.com/metabase/metabase/f2816df9b3f9731a38f55aeac3c8541a193965e8/src/metabase/api/card.clj"))
  (println 
    (cond
      (empty? args)
      (page/merge-base-diff "HEAD")
     
      (= 1 (count args))
      (page/merge-base-diff (first args))
     
      (= 3 (count args))
      (let [[owner repo numstr] args
            num (Integer/parseInt numstr)]
        (page/pull-request-diff owner repo num))
     
      :else "give <reference> e.g. HEAD to compare with origin/master, or give <owner repo number> for a public github PR"))
  (shutdown-agents))
