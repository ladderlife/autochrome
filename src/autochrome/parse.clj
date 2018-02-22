(ns autochrome.parse
  (:require [autochrome.common :refer [open->closed closed->open]]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(defn ns->source-string
  [ns]
  (when-let [path (some-> ns ns-publics first val meta :file)]
    (-> path io/resource slurp)))

(defn clj-whitespace
  [c]
  (case c
    ;; (\, \space \newline \tab)
    (\, \space  \tab) true
    false))

(defn clj-special
  [c]
  (case c
    (\( \) \{ \} \[ \] \, \space \newline \tab \") true
    false))

(defn whitespace-string
  [{:keys [^String buf pos]}]
  (loop [idx pos]
    (if (and (< idx (.length buf)) (clj-whitespace (.charAt buf idx)))
      (recur (inc idx))
      (.substring ^String buf pos idx))))

(defn symbol-string
  [{:keys [^String buf pos]}]
  (loop [idx pos]
    (if (and (< idx (.length buf)) (not (clj-special (.charAt buf idx))))
      (recur (inc idx))
      (.substring ^String buf pos idx))))

(defn count-newlines-in-string
  [s]
  (let [n (volatile! 0)]
    (dotimes [i (.length s)]
      (when (= \newline (.charAt s i))
        (vswap! n inc)))
    @n))

(def ^:dynamic *line-number* (atom 0))

(defn lex
  [orig-ctx]
  (loop [{:keys [^String buf pos] :as ctx} (transient orig-ctx)
         tokens (transient [])]
    (if (>= pos (.length buf))
      (persistent! tokens)
      (case (.charAt buf pos)
        \newline
        (recur (assoc! ctx :pos (inc pos))
               (conj! tokens \newline))

        (\, \space \tab)
        (let [^String whitespace (whitespace-string ctx)]
          (recur (assoc! ctx :pos (+ pos (.length whitespace)))
                 (conj! tokens {:type :ws :text whitespace})))

        \# (let [nt (.charAt buf (inc pos))]
             (if (= \_ nt)
               (recur (assoc! ctx :pos (+ 2 pos)) (conj! tokens "#_"))
               (recur (assoc! ctx :pos (inc pos))
                      (conj! tokens (.charAt buf pos)))))

        (\( \) \{ \} \[ \] \` \~ \^ \')
        (recur (assoc! ctx :pos (inc pos))
               (conj! tokens (.charAt buf pos)))

        \" (let [string-end (loop [i (inc pos)
                                   escaped false]
                              (cond
                                (and (not escaped) (= \" (.charAt buf i)))
                                (inc i)

                                (= \\ (.charAt buf i))
                                (recur (inc i) (not escaped))

                                :else
                                (recur (inc i) false)))
                 text (.substring buf pos string-end)]
             (recur (assoc! ctx :pos string-end)
                    (conj! tokens {:type :string :text text :nlines (count-newlines-in-string text)})))

        \; (let [comment-end (loop [i pos]
                               (cond
                                 (>= i (.length buf))
                                 i
                                 ;; coalesce multiline comments
                                 (= \newline (.charAt buf i))
                                 (let [past-end? (>= (inc i) (.length buf))]
                                   (if (and (not past-end?)
                                            (= \; (.charAt buf (inc i))))
                                     (recur (+ 2 i))
                                     i))
                                 :else (recur (inc i))))
                 comment-text (.substring buf pos comment-end)]
             (recur (assoc! ctx :pos comment-end)
                    (conj! tokens {:type :ws
                                   :subtype :comment
                                   :nlines (count-newlines-in-string comment-text)
                                   :text comment-text})))

        \\ (let [literal-end (loop [i (+ 2 pos)]
                               (if (clj-special (.charAt buf i))
                                 i
                                 (recur (inc i))))]
             (recur (assoc! ctx :pos literal-end)
                    (conj! tokens {:type :char-literal :text (.substring buf pos literal-end)})))

        \@ (recur (assoc! ctx :pos (inc pos))
                  (conj! tokens (.charAt buf pos)))

        \: (let [^String sym (symbol-string ctx)]
             (recur (assoc! ctx :pos (+ pos (.length sym)))
                    (conj! tokens {:type :keyword :text sym})))

        (let [^String sym (symbol-string ctx)]
          (recur (assoc! ctx :pos (+ pos (.length sym)))
                 (conj! tokens {:type :symbol :text sym})))))))

(def ^:const clojure-core-ns (find-ns 'clojure.core))

(declare parse-list)

(declare -parse-one)

;; 'decoration' precedes one form eg deref, quote, data reader etc.
(defn parse-decoration
  [base ts]
  ;; collect leading whitespace
  (loop [ws []
         [t & more :as ts] ts]
    (cond
      (= :ws (:type t))
      (recur (conj ws t) more)

      (char? t)
      (let [{:keys [val rest]} (-parse-one ts)]
        {:val (-> (assoc base :wscontents (conj ws val))
                  (assoc :contents [val]))
         :rest rest})

      (map? t)
      {:val (-> (assoc base :wscontents (conj ws t))
                (assoc :contents [t]))
       :rest more})))

(defn collect-hash-unders
  [ts]
  (loop [n 0
         hashes []
         [t & more :as ts] ts]
    (cond
      (= t "#_") (recur (inc n) (conj hashes t) more)
      (= :ws (:type t)) (recur n (conj hashes (:text t)) more)
      :else {:hashes hashes :n n :rest ts })))

(defn next-n-forms
  [n ts]
  (loop [n n
         forms []
         ts ts]
    (cond
      (or (zero? n) (empty? ts)) {:forms forms :rest ts}
      :else
      (let [{:keys [val rest]} (-parse-one ts)]
        (recur
         (if (not= :ws (:type val)) (dec n) n)
         (conj forms val) rest)))))

(defn ignore-whitespace
  [ts]
  (loop [ts ts]
    (if-not (= :ws (:type (first ts)))
      ts
      (recur (next ts)))))

;; :contents needs to be a list, due to object identity shenanigans in DiffContext
(defn vec->list
  [v]
  (loop [head nil
         idx (dec (count v))]
    (if (< idx 0)
      head
      (recur (cons (nth v idx) head) (dec idx)))))

(defn nows
  [forms]
  (vec->list (filterv #(not= :ws (:type %)) forms)))

(defn -parse-one
  [ts]
  (when-let [t (first ts)]
    (if-let [closing-delimiter (open->closed t)]
      (parse-list closing-delimiter (next ts))
      (case t
        \#
        (let [ts (next ts)
              nt (first ts)]
          ;; dispatch
          (case nt
            ;; \_ (-> (parse-one nt) :rest parse-one)
            \( (let [{:keys [val rest]} (-parse-one ts)]
                 {:val {:type :lambda :text val} :rest rest})
            \{ (assoc-in (-parse-one ts) [:val :delim] "#{")

            \' (let [{:keys [val rest]} (-parse-one (next ts))]
                 {:val {:type :var-quote :text (:text val)} :rest rest})

            ;; reader conditional
            {:type :symbol :text "?"}
            (let [{:keys [val rest]} (-parse-one (ignore-whitespace (next ts)))]
              {:val (assoc val :type :reader-conditional) :rest rest})

            {:type :symbol :text "?@"}
            (let [{:keys [val rest]} (-parse-one (ignore-whitespace (next ts)))]
              {:val (assoc val :type :reader-conditional-splicing) :rest rest})

            (case (:type nt)
              :string
              (let [{:keys [val rest]} (-parse-one ts)]
                {:val {:type :regex :text (:text val)} :rest rest})
              :symbol
              (let [{:keys [val rest]} (-parse-one ts)]
                (parse-decoration
                 {:type :data-reader :text (:text nt)}
                 (next ts)))
              (throw (ex-info "bad dispatch form" {:bad-token nt})))))

        "#_"
        (let [{:keys [n hashes rest]} (collect-hash-unders ts)
              {:keys [forms rest]} (next-n-forms n rest)]
          {:val {:type :hash-under
                 :hashes hashes
                 :wscontents forms
                 :contents (nows forms)}
           :rest rest})

        \^ (parse-decoration {:type :meta} (next ts))
        \@ (parse-decoration {:type :deref} (next ts))
        \~ (if (= \@ (second ts))
             (parse-decoration {:type :unquote-splicing} (nnext ts))
             (parse-decoration {:type :unquote} (next ts)))
        \` (parse-decoration {:type :syntax-quote} (next ts))

        ;; quote has :val instead of :contents because most of the time
        ;; you don't want to recurse into quote forms
        \' (let [{:keys [val rest]} (-parse-one (next ts))]
             {:val {:type :quote :val (list val)} :rest rest})

        \newline (do (swap! *line-number* inc)
                     {:val {:type :ws :text "\n"}
                      :rest (next ts)})
        (do
          (when-let [nlines (:nlines t)]
            (swap! *line-number* + nlines))
          {:val t :rest (next ts)})))))

(defn parse-list
  [closer ots]
  (loop [forms []
         ts ots]
    (if-let [t (first ts)]
      (if-let [sub-closer (open->closed t)]
        (let [{:keys [val rest]} (parse-list sub-closer (next ts))]
          (recur (conj forms val) rest))
        (if (= closer t)
          {:val {:type :coll
                 :delim (closed->open closer)
                 :wscontents forms
                 :contents (nows forms)}
           :rest (next ts)}
          (let [{:keys [val rest]} (-parse-one ts)]
            (recur (if (vector? val)
                     (into forms val)
                     (conj forms val))
                   rest))))
      (let [msg {:msg "expecting closer" :closer closer :ts ts :forms forms}]
        (println (pr-str msg))
        #_(fipp.edn/pprint forms)
        (throw (ex-info "expecting closer" {:closer closer :ts ts}))))))

(defn parse-many
  [ts]
  (binding [*line-number* (atom 1)]
    (loop [forms []
           ts ts]
      (if (some? ts)
        (let [start-line @*line-number*
              {:keys [val rest]} (-parse-one ts)]
          (recur
           (cond
             (vector? val) (into forms val)
             (map? val) (conj forms (-> val
                                        (assoc :start-line start-line)
                                        (assoc :lines (inc (- @*line-number* start-line)))))
             :else (conj forms val))
           rest))
        forms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse
  [s]
  {:pre [(string? s)]}
  (let [wscontents (parse-many (lex {:pos 0 :buf s}))]
    {:type :root
     :wscontents wscontents
     :contents (nows wscontents)}))

(defn parse-one
  [s]
  (-> s parse :contents first))

(declare render*)

(defn render-contents
  [the-form ->cts]
  (let [rendered (for [f (->cts the-form)] (render* f ->cts))]
    (apply str (if (= :contents ->cts)
                 (interpose " " rendered)
                 rendered))))

(defn render*
  [t ->cts]
  (cond
    (sequential? t) (apply str (for [f (->cts t)] (render* f ->cts)))
    (string? t) t
    :else
    (case (:type t)
      (:ws :symbol :string :comment :keyword) (:text t)
      :data-reader (str "#" (:text t))
      :regex (str "#" (:text t))
      :char-literal (:text t)
      :meta (apply str "^" (for [f (->cts t)] (render* f ->cts)))
      :quote (str "'" (render* (:val t) ->cts))
      :syntax-quote (apply str "`" (for [f (->cts t)] (render* f ->cts)))
      :unquote (apply str "~" (for [f (->cts t)] (render* f ->cts)))
      :deref (apply str "@" (for [f (->cts t)] (render* f ->cts)))
      :var-quote (str "#'" (:text t))
      :hash-under (str (string/join (:hashes t))
                       (render-contents t ->cts))

      :coll
      (str (:delim t)
           (render-contents t ->cts)
           (open->closed (:delim t)))

      :lambda  (str "#" (render* (:text t) ->cts))
      :root (apply str
                   (for [f (->cts t)]
                             (render* f ->cts)))

      (case t
        (\` \~ \@) t
        (pr-str t)))))

(defn render
  [t]
  (render* t :contents))

(defn render-dup
  [t]
  (render* t :wscontents))
