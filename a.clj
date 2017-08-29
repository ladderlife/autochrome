(ns a
  (:require [clojure.java.io :as io]
            [cljs.reader :as cljs-reader]
            [clojure.tools.reader :as tools-reader]
            [clojure.tools.reader.reader-types :as rt]
            [clojure.edn :as edn]
            [hiccup.core :as h])
  (:import [java.nio.file Files]))


(defn ns->source-string
  [ns]
  (when-let [path (some-> ns ns-publics first val meta :file)]
    (println "path = " path)
    (if (.startsWith path "/")
      (slurp path)
      (-> path io/resource slurp))))

(defn clj-whitespace
  [c]
  (case c
    ;; (\, \space \newline \tab)
    (\, \space  \tab) true
    false))

(defn clj-special
  [c]
  (case c
    (\( \) \{ \} \[ \] \, \space \newline \tab) true
    false))

(defn whitespace-string
  [{:keys [buf pos]}]
  (loop [idx pos]
    (if (and (< idx (.length buf)) (clj-whitespace (.charAt buf idx)))
      (recur (inc idx))
      (.substring ^String buf pos idx))))

(defn symbol-string
  [{:keys [buf pos]}]
  (loop [idx pos]
    (if (and (< idx (.length buf)) (not (clj-special (.charAt buf idx))))
      (recur (inc idx))
      (.substring ^String buf pos idx))))

(defn lex
  [orig-ctx]
  (loop [{:keys [buf pos line] :as ctx} (-> orig-ctx (assoc :line 1) transient)
         tokens (transient [])]
    (if (>= pos (.length buf))
      (persistent! tokens)
      (case (.charAt buf pos)
        \newline
        (recur (assoc! ctx :line (inc line) :pos (inc pos))
               (conj! tokens {:type :ws :text "\n" :line line}))

        (\, \space \tab)
        (let [whitespace (whitespace-string ctx)]
          (recur (assoc! ctx :pos (+ pos (.length whitespace)))
                 (conj! tokens {:type :ws :text whitespace})))
        
        (\( \) \{ \} \[ \] \` \~)
        (recur (assoc! ctx :pos (inc pos))
               (conj! tokens (.charAt buf pos)))

        \" (let [string-end (loop [i (inc pos)
                                   escaped false]
                              (cond
                                (= i (.length buf))
                                i

                                (and (not escaped) (= \" (.charAt buf i)))
                                (inc i)

                                (= \\ (.charAt buf i))
                                (recur (inc i) true)

                                :else
                                (recur (inc i) false)))]
             (recur (assoc! ctx :pos string-end)
                    (conj! tokens {:type :string :text (.substring buf pos string-end)})))

        \; (let [comment-end (loop [i pos]
                               (if (and (< i (.length buf)) (not= \newline (.charAt buf i)))
                                 (recur (inc i))
                                 i))]
             (recur (assoc! ctx :pos comment-end)
                    (conj! tokens {:type :comment :text (.substring buf pos comment-end)})))
        
        (let [sym (symbol-string ctx)]
          (recur (assoc! ctx :pos (+ pos (.length sym)))
                 (conj! tokens {:type :symbol :text sym})))))))

(defn parse-symbol
  [{:keys [text] :as sym}]
  (try
    (let [rd (read-string text)]
      (if (keyword? rd)
        (assoc sym :val rd)
        (try
          (assoc sym :val rd :resolved (resolve rd))
          (catch Exception e
            (assoc sym :val rd)))))
    (catch Exception e
      sym)))

(defn javadoc-url
  [cls]
  ;; https://docs.oracle.com/javase/8/docs/api/java/lang/String.html
  (str "https://docs.oracle.com/javase/8/docs/api/"
       (.replace (.getName cls) \. \/)
       ".html"))

(def ^:const clojure-core-ns (find-ns 'clojure.core))
(meta (resolve 'javadoc-url))

(defn render-keyword
  [text]
  (if (.startsWith text "::")      ;we can't touch namespaced keywords
    [:span {:class :keyword} text]
    (let [slash-idx (.indexOf text "/")]
      (if (< slash-idx 0)
        [:span {:class :keyword} text]
        (let [kw (edn/read-string text)]
          #_(println text kw (namespace kw) (find-ns (symbol (namespace kw))))
          [:span {:class :keyword}
           ":"
           [:span {:class :ns-ref} (.substring text 1 slash-idx)]
           (.substring text slash-idx)]
          #_[:span
           [:span {:class :keyword} ":"]
           [:span {:class :ns-ref} (.substring text 1 slash-idx)]
           [:span {:class :keyword} (.substring text slash-idx)]])))))

(defn add-link
  [text meta]
  (cond
    (= clojure-core-ns (:ns meta))
    [:a {:href (format "https://clojuredocs.org/clojure.core/%s" (:name meta))} text]
    
    (:file meta)
    [:a {:href (str (ns-name (:ns meta)) ".html#L" (:line meta))} text]

    :else text
    ))

(defn render-symbol
  [{:keys [val resolved text]}]
  (cond
    (nil? val)
    [:span {:class :unparsed} text]

    (keyword? val)
    (render-keyword text)
    
    (class? resolved)
    [:span {:class :java-class} text]

    (meta resolved)
    (let [m (meta resolved)
          core? (= (find-ns 'clojure.core) (:ns m))
          text (add-link text m)
          classes (cond-> [:var-ref]
                    (:macro m) (conj :macro)
                    core? (conj :clojure-core))]
      [:span {:class (clojure.string/join " " (mapv name classes))} text])
    
    (.startsWith text ".")
    [:span {:class :macro} text]

    :else
    (or
      (when-let [cls (and
                       (.endsWith text ".")
                       (some-> text (.substring 0 (dec (.length text))) symbol resolve))]
        [:span {:class :java-class} [:a {:href (javadoc-url cls)} text]])
      [:span {:class :unknown} text])))


(declare parse-list)

(def open->closed
  {\( \)
   \[ \]
   \{ \}})

(def closed->open
  {\) \(
   \] \[
   \} \{})

(defn parse-one
  [ts]
  (when-let [t (first ts)]
    (if-let [closing-delimiter (open->closed t)]
      (parse-list closing-delimiter (next ts))
      {:val t :rest (next ts)})))

(defn parse-list
  [closer ts]
  (loop [forms []
         ts ts]
    (if-let [t (first ts)]
      (if-let [sub-closer (open->closed t)]
        (let [{:keys [val rest]} (parse-list sub-closer (next ts))]
          (recur (conj forms val) rest))
        (if (= closer t)
          {:val {:type :coll :delim (closed->open closer) :contents forms}
           :rest (next ts)}
          (recur (conj forms t) (next ts))))
      (throw (ex-info "expecting closer" {:closer closer :ts ts :lastform (last forms)})))))

(defn render-parse
  [t]
  (case (:type t)
    :ws      (:text t)
    :symbol  (render-symbol (parse-symbol t))
    :string  [:span {:class :string} (:text t)]
    :comment [:span {:class :comment} (:text t)]
    :coll 
   (into [:span (:delim t)]
      (conj
        (mapv render-parse (:contents t))
        (open->closed (:delim t))))))

(defn render-code
  [src]
  (when src
    (let [lexed (lex {:pos 0 :buf (str "[" src "]")})
          parsed (-> (parse-one lexed) :val :contents)]
     (h/html
      [:head [:link {:rel :stylesheet :href "file:///Users/russell/a/code.css"}]
       [:div {:class :container}
        [:pre (into [:code {:class "gutter"}]
                    (for [line (range 1 (inc (count (clojure.string/split-lines src))))]
                      [:span {:class :punctuation :id (format "L%d" line)} (format "%04d\n" line)]))]
        [:div {:style "width:1px;"}]
        [:pre {:class "source"} (into [:code] (mapv render-parse parsed))]]]))))


(let [lexed (time (lex {:pos 0
                        :buf (str "[" (-> "clojure/core.clj" io/resource slurp) "]")}))
      parsed (time (-> (parse-one lexed) :val :contents))]
  {:tokens (count lexed)
   :forms (count parsed)})

