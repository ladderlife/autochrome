(ns autochrome.annotation
  (:require [autochrome.common :as clj-common]
            [autochrome.scope :as scope]
            [autochrome.tree :as tree]
            [autochrome.xref :as xref])
  (:import [java.util IdentityHashMap]))

(defn attach
  [{:keys [type text delim wscontents] :as form} ann]
  (let [a (.get ann form)
        rec (cond
              (or (= type :coll) (clj-common/decoration? form))
              (assoc form :wscontents (mapv #(attach % ann) wscontents))

              (= type :quote)
              (assoc form :val (list (attach (first (:val form)) ann)))

              (= type :lambda)
              (assoc form :text (attach text ann))

              :else form)]
    (cond-> rec
      a (assoc :annotation a))))

(defn annotated?
  [form ann]
  (or
   (.get ann form)
   (when-let [children (tree/->children form)]
     (loop [[c & cs] children]
       (when c
         (if (annotated? c ann)
           true
           (recur cs)))))))

(defn syntax-highlighting
  [form]
  (let [ann (IdentityHashMap.)]
    (scope/execute-writer
     (scope/walk-with-scope
      form
      (fn [c f]
        (if-let [sym (scope/form->real-symbol c f)]
          (.put ann f
                (let [locally-bound (get (:scope c) sym)
                      qual (scope/qualify-symbol c f)]
                  (cond
                    (get (:scope c) sym)
                    :local

                    (and (nil? qual) (xref/javadoc-link (:text f)))
                    :java-class

                    (and (symbol? qual) (= "clojure.core" (namespace qual)))
                    :core

                    :else sym)))
          (when (some-> f :text (.startsWith "."))
            (.put ann f :java-class))))
      scope/default-context))
    ann))
