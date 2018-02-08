(ns autochrome.components
  (:require [autochrome.common :as clj-common :refer [special-form? open->closed]]
            [om.dom :as dom :refer [span]]))

(declare form)

(defmacro defcomponent
  [name & [[props children] & body]]
  `(defn ~name [& args#]
     (let [[~props & ~children] args#]
       ~@body)))

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

(defn xref-clojure-core
  [text]
  (let [macro? (or (some-> text symbol resolve meta :macro)
                   (special-form? text))]
    (dom/a {:href (clojure-core-link text)}
           (span {:className (if macro? "macro clojure-core" "clojure-core")}
                 text))))

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

(defn xref-java-lang
  [text]
  (let [link (javadoc-link text)]
    (cond->> (dom/span {:className "java-class"} text)
      link (dom/a {:href link}))))

(defcomponent symbol-component
  [{a :annotation :keys [text xref] :as the-form} _]
  ;; not much to do here now
  (if xref
    (dom/a {:href xref} (symbol-component (dissoc the-form :xref)))
    (cond
      (nil? a) text
      ;; do something more interesting with locals?
      (= :local a) text
      (= :java-class a) (xref-java-lang text)
      (= :core a) (xref-clojure-core text)
      (= :broken a) (span {:className "unparsed"} text)
      (= :shead a) (do (println 'shead!)
                       (span {:className "shead"} text))
      (= :thead a) (do (println 'thead!)
                       (span {:className "thead"} text))
      (keyword? a) (span {:className (str (name a))} text)
      (namespace a) (dom/span {} (dom/span {:className "ns-ref"} (namespace a))
                              "/" (dom/span {:className "var-ref"} (name a)))
      (symbol? a) (dom/span {:className "var-ref"} text)
      :else (throw (ex-info "unrecognized annotation" the-form)))))

(declare form)

(defcomponent form
  [{:keys [type text annotation] :as the-form} _]
  (let [form-annotation? (contains? #{:added :deleted :shead :thead} annotation)
        the-form (cond-> the-form form-annotation? (dissoc :annotation))
        rendered
        (case type
          :newline (span "\n")
          :ws      (if (= :comment (:subtype the-form))
                     (span {:className "comment"} text)
                     text)
          :symbol  (symbol-component the-form)

          :keyword      (span {:className "keyword"} text)
          :string       (span {:className "string"} text)
          :data-reader  (span {:className "metadata"}
                              "#"
                              (:text the-form)
                              (mapv form (:wscontents the-form)))
          :regex        (span {} "#" (span {:className "string"} text))
          :char-literal (span {:className "string"} text)
          :quote        (span {} "'" (form (first (:val the-form))))
          :var-quote    (span {:className "var-ref"} "#'" text)
          ;; TODO hash-under
          (:deref :syntax-quote :unquote :unquote-splicing)
          (span {}
                (clj-common/->decorator the-form)
                (form (first (:contents the-form))))

          :coll
          (span {}
                (str (:delim the-form))
                (conj (mapv form (:wscontents the-form))
                      (str (open->closed (:delim the-form)))))

          :lambda
          (span {} "#" (form (:text the-form)))

          :reader-conditional
          (span {} "#?(" (mapv form (:wscontents the-form)) ")")

          :reader-conditional-splicing
          (span {} "#?@(" (mapv form (:wscontents the-form)) ")")

          (span {:className "unparsed"} (pr-str the-form)))

        rendered (if-let [m (:meta the-form)]
                   (dom/span {} "^"
                             (form m)
                             (:text (:meta-ws the-form))
                             (form (dissoc the-form :meta)))
                   rendered)]
    (cond->> rendered
      form-annotation? (dom/span {:className (str (name annotation))}))))

(defn line-numbers
  [{:keys [lines start-line linkbase] :as the-form}]
  (for [i (range start-line (+ start-line lines))
        :let [;; make up a unique id so the browser doesn't try to scroll
              uid (str (.substring linkbase (- (count linkbase) 5)) i)]]
    (cond->> (dom/div {} (str i))
      linkbase (dom/a {:href (str linkbase i)}))))

(defcomponent code
  [{:keys [lines start-line linkbase things annotation id] :as the-form} children]
  (dom/div
    {:className "code-card"
     :id id}
    (dom/div {:className "code-card-heading"}
      (first children)
      (dom/div {:className "code-card-heading-extra"} (rest children)))
    (dom/div {:className "container"}
      (dom/pre {:className "gutter"}
        (dom/code {:className "punctuation"} (line-numbers the-form)))
      (dom/div {:style {:width "1px"}})
      (dom/pre
        {:className
         (cond-> "source"
           annotation (str " " (name annotation)))}
        (for [th things]
          (if (map? th) (form th) th))))))

(defcomponent top-level-comment
  [{:keys [lines start-line text] :as the-form} _]
  (dom/div
   {:className "code-card top-comment"}
   (dom/div {:className "container"}
            (dom/pre {:className "gutter"}
                     (dom/code
                      {:className "punctuation"}
                      (line-numbers the-form)))
            (dom/div {:style {:width "1px"}})
            (dom/pre {:className "source"}
                     (dom/span {:className "comment"} text)))))

(defcomponent panes
  [props children]
  (let [[left right] children]
    (dom/div
     {:style {:display "flex"
              :flex-direction "row"}}
     (dom/div {:className "diffpane"} left)
     (dom/div {:className "diffpane"} right))))

(defcomponent root
  [_ children]
  (dom/div
   {:style {:display "flex"
            :flex-direction "column"}}
   children))

(defn heading
  [text]
  (dom/div {:className "filename-heading"} text))

(defn spacer
  []
  (dom/div {:className "spacer"}))
