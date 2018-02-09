(ns autochrome.xref
  (:require [autochrome.common :as clj-common]))

(def ^:const special->doc
  {"set!" "https://clojure.org/reference/vars#set"
   "catch" "https://clojure.org/reference/special_forms#try"
   "finally" "https://clojure.org/reference/special_forms#try"})

(defn clojure-core-link
  [text]
  (if (clj-common/special-form? text)
    (or (special->doc text)
        (str "https://clojure.org/reference/special_forms#" text))
    (str "https://clojuredocs.org/clojure.core/" text)))

(defn javadoc-link
  ([text] (or (javadoc-link "java.lang" text)
              (javadoc-link "java.util" text)
              (javadoc-link "java.io" text)))
  ([package text]
   (when (Character/isUpperCase (.charAt text 0))
     (try
       (let [text (if (.endsWith text ".")
                    (.substring text 0 (dec (count text)))
                    text)
             classname (str package "." text)]
         (Class/forName classname)
         (str "https://docs.oracle.com/javase/8/docs/api/"
              (.replace classname "." "/")
              ".html"))
       (catch ClassNotFoundException e nil)))))

