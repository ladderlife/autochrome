(ns autochrome.scope
  (:require [autochrome.common :as clj-common]
            [autochrome.parse :as parse]
            [clojure.test :refer [deftest is are]])
  (:import [java.io Writer]))

;; crude Writer monad (can't be nested artibrarily/lexically)
;; The only reason this needs to work with pseudo-writer-monad instead
;; of simply returning the results like it used to do, is because
;; you can depend on things while destructuring if you use :or.
;; This means that `restructure` has to both return a value (the bound names)
;; and also potentially emit results (:or deps).  So everything had to be changed.

(def ^:dynamic *writer-output* nil)

(defn write!
  "Emit a value to the output/stream/log"
  [v]
  (when v
    (vswap! *writer-output* conj! v)))

(defmacro execute-writer
  "run body and discard the result.  returns vector of all values written"
  [& body]
  `(binding [*writer-output* (volatile! (transient []))]
     ~@body
     (persistent! @*writer-output*)))

(defn key->bound-sym
  "`{:keys [foo/bar]}` binds `bar`"
  [{:keys [text]}]
  (or (some->> text (re-find #".+?/(.+)") second) text))

;; mostly just to keep track of all the fields...
(defrecord Context [root? scope globals lang alias quote-depth])

(def default-context
  (map->Context {:root? true
                 :globals (clj-common/clojure-core-scope)
                 :scope {}
                 :quote-depth 0}))

(defmethod print-method Context
  [ctx ^Writer w]
  (print-method (into {} ctx) w))

(defn add-to-scope
  "add local binding(s) to the current context scope"
  [^Context ctx name which info]
  (cond
    (or (nil? name) (pos? (:quote-depth ctx))) ctx
    ;(empty? name) (throw (ex-info "empty name" {:context (dissoc ctx :globals)}))
    (sequential? name) (reduce (fn [a n] (add-to-scope a n which info)) ctx name)
    (map? name)        (update ctx which assoc (symbol (:text name)) info)
    :else              (update ctx which assoc (symbol name) info)))

(declare walk-with-scope)

(defn walk-body
  "when you don't know what else to do"
  [forms func ^Context ctx]
  (when (nil? ctx)
    (throw (ex-info "nil ctx" {:forms forms})))
  (when (map? forms)
    (throw (ex-info "walk-body expectes sequence of forms, use walk-with-scope for single forms")))
  (doseq [form forms]
    (walk-with-scope form func ctx)))

(defn restructure
  [{:keys [type text delim contents] :as binding-form} func ^Context ctx]
  (when binding-form
    (when-not (map? binding-form) (throw (ex-info "restructure expects forms" {:not-a-form binding-form})))
    (cond
      (= :symbol type) (list text)
      ;; map destructuring
      (and (= :coll type) (= \{ delim))
      (mapcat
       (fn [[binding-key binding-val]]
         (if (not= :keyword (:type binding-key))
           (restructure binding-key func ctx)
           (case (:text binding-key)
             ":keys" (map key->bound-sym (:contents binding-val))
             ":strs" (map :text (:contents binding-val))
             ":as"   (list (:text binding-val))
             ":or"   (walk-body (take-nth 2 (rest (:contents binding-val))) func ctx)
             (throw (ex-info (str "unsupported map destructuring (" (:text binding-key) ")")
                             {:binding-form binding-form
                              :rendered (parse/render binding-form)})))))
       (partition 2 contents))
      ;; vector destructuring
      (and (= :coll type) (= \[ delim))
      (loop [bound          []
             [form & forms] contents]
        (cond (nil? form)                           bound
              (= form {:type :keyword :text ":as"}) (recur (conj bound (:text (first forms))) (next forms))
              (= form {:type :symbol :text "&"})    (recur bound forms)
              :else                                 (recur (into bound (restructure form func ctx)) forms))))))

(defn local
  [^Context ctx arg]
  (add-to-scope ctx arg :scope :local))

(defn +bindings
  [form func ^Context ctx]
  (local ctx (restructure form func ctx)))

(defn walk-fnspec
  "fnspec is like `([x] (inc x))`"
  [[argv & body] func ^Context ctx]
  (walk-body body func (+bindings argv func ctx)))

(defn walk-bindings*
  "each binding in a let is in scope for subsequent bindings"
  [bvec body func ^Context context]
  (loop [[b & bs] (partition 2 (:contents bvec))
         ctx      context]
    (if (nil? b)
      (walk-body body func ctx)
      (let [[name expr] b]
        ;; might as well handle these here (for, doseq, etc)
        (case (:text name)
          ":let"             (recur (concat (partition 2 (:contents expr)) bs) ctx)
          (":when" ":while") (do (walk-with-scope expr func ctx)
                                 (recur bs ctx))
          ;; normal let binding
          (do (walk-with-scope expr func ctx)
              (recur bs (+bindings name func ctx))))))))

;;; `binding` is different from `let` because the LEFT hand side
;;; can contain dependencies as well as the right
(defn walk-literal-binding-form
  [bvec body func ^Context context]
  (loop [[b & bs] (partition 2 (:contents bvec))
         ctx      context]
    (if (nil? b)
      (walk-body body func ctx)
      (let [[name expr] b]
        (do (walk-body [name expr] func ctx)
            (recur bs (+bindings name func ctx)))))))

(defn walk-fn-form
  [[first-param & more :as contents] func ^Context ctx]
  (if (= :symbol (:type first-param))
    (recur more func (+bindings first-param func ctx))
    (cond
      (= \[ (:delim first-param)) (walk-fnspec contents func ctx)
      (= \( (:delim first-param)) (doseq [form more] (walk-fnspec (:contents form) func ctx))
      (nil? name) (throw (ex-info "unsupported fn form?" {:contents contents}))
      :else (walk-body more func ctx))))

(defn walk-defn-form
  [{[_ name & body] :contents} func ^Context ctx]
  ;; ignore docstring (we don't care about any strings really)
  (let [body (filter #(not= :string (:type %)) body)
        [argv? & more] body
        fnscope (+bindings name func ctx)]
    (case (:delim argv?)
      \[ (walk-fnspec body func fnscope)
      \( (doseq [form body] (walk-fnspec (:contents form) func fnscope))
      \{ (do (walk-body (:contents argv?) func fnscope)
             (walk-defn-form {:contents (list* 'defn name more)} func ctx))
      (throw (ex-info "wtf defn" {:name name})))))

(defn walk-defmacro-form
  [form func ^Context ctx]
  (walk-defn-form form func (local ctx ["&env" "&form"])))

(defn walk-defprotocol-form
  [{[_ & entire-body] :contents :keys [delim] :as form} func ^Context ctx]
  (let [[name & body] (filter #(not= :string (:type %)) entire-body)]
    #_(walk-with-scope name func ctx)
    (doseq [{[name & fnspec] :contents} body]
      (walk-fnspec fnspec func (+bindings name func ctx)))))

(defn walk-defrecord-form
  [{[_ & body] :contents :keys [delim] :as form} func ^Context ctx]
  (let [[name fields & body] (filter #(not= :string (:type %)) body)
        record-ctx (+bindings name func (+bindings fields func ctx))]
    (doseq [{:keys [contents] :as elem} body]
      (if contents
        ;; hopefully an fnspec
        (walk-fnspec (rest contents) func record-ctx)
        ;; hopefully a protocol name
        (write! (func ctx elem))))))

;; (defmethod method dispatch-val [args] body)
(defn walk-defmethod-form
  [{[_ name dispatch-val & fnspec] :contents :keys [delim] :as form} func ^Context ctx]
  (do (walk-with-scope name func ctx) ; defmethod depends on the defmulti
      (walk-fnspec fnspec func ctx)))

(defn walk-letfn-form
  [{[_ bindings & body] :contents} func ^Context ctx]
  (let [names? (mapcat #(restructure (first (:contents %)) func ctx) (:contents bindings))
        letfn-scope (local ctx names?)]
    (doseq [form (:contents bindings)]
      (walk-fn-form (rest (:contents form)) func letfn-scope))
    (walk-body body func letfn-scope)))

(defn ->fq-symbol
  [scope alias s]
  (if-let [s-ns (some-> s (namespace) (symbol))]
    (symbol
     (name (or (get alias s-ns)
               s-ns))
     (name s))
    (get scope s)))

(defn qualify-symbol
  [{:keys [globals alias]} {:keys [text] :as form}]
  (->fq-symbol globals alias (symbol text)))

(defn walk-case-form
  [{[_ expr & cases] :contents} func ^Context ctx]
  ;; test constants in `case` forms are implicitly quoted
  (walk-with-scope expr func ctx)
  (doseq [[const expr] (partition-all 2 cases)]
    (walk-with-scope (or expr const) func ctx)))

(defn form->func
  "return fully-qualified symbol in funcall position of form"
  [form ^Context ctx]
  (let [func (first (:contents form))]
    (when (and (= \( (:delim form))
               (= :symbol (:type func))
               (zero? (:quote-depth ctx)))
      (qualify-symbol ctx func))))

(defn walk-def-form
  [{[_ name & more] :contents} func ^Context ctx]
  (walk-body more func ctx))

(defn walk-om-defui-form
  [{[_ name & body] :contents :as form} func ^Context ctx]
  (let [forms-by-type (group-by :type body)]
    #_(when (> (count forms-by-type) 2)
        (throw (ex-info "I do not like your defui" {:types (keys forms-by-type)})))
    (doseq [proto (forms-by-type :symbol)
            :when (not (contains? #{"static" "Object"} (:text proto)))]
      (walk-with-scope proto func ctx))
    (doseq [impl (forms-by-type :coll)]
      (walk-fnspec (next (:contents impl)) func ctx))))

(def ^:const reader-lambda-scope
  (into {} (map #(vector (symbol (str "%" %)) :implicit)
                (list* "" "&" (range 1 10)))))

(defn top-level-walker-fn
  "given fully-qualified function/macro name, return fn to correctly walk top-level form"
  [sym]
  (case sym
    (clojure.core/defn
      clojure.core/defn-)    walk-defn-form
    (clojure.core/defrecord
     clojure.core/deftype
     potemkin/defprotocol+)  walk-defrecord-form
    clojure.core/defmacro    walk-defmacro-form
    clojure.core/defprotocol walk-defprotocol-form
    clojure.core/defmethod   walk-defmethod-form
    (clojure.core/def
      clojure.core/defonce
      clojure.test/deftest)  walk-def-form
    (clojure.core/comment
      clojure.core/declare
      clojure.core/quote)    (constantly [])

    nil))

(defn walk-binding-form
  "try to walk a form which can have bindings - returns nil if form is not recognized as a binding form"
  [{[func-pos & params :as contents] :contents :keys [delim] :as form} func ^Context ctx]
  (walk-with-scope func-pos func ctx)
  (let [dispatch-form (form->func form ctx)]
    (case dispatch-form
      (clojure.core/for clojure.core/doseq clojure.core/dotimes clojure.core/let clojure.core/loop
                        clojure.core/when-let clojure.core/if-let clojure.core/if-some clojure.core/when-some)
      (walk-bindings* (first params) (rest params) func ctx)

      (clojure.core.async/go-loop manifold.deferred/let-flow)
      (do (walk-with-scope func-pos func ctx)
          (walk-bindings* (first params) (rest params) func ctx))

      clojure.core/recur   (walk-body params func ctx)
      clojure.core/fn      (walk-fn-form params func ctx)
      clojure.core/case    (walk-case-form form func ctx)
      clojure.core/letfn   (walk-letfn-form form func ctx)
      clojure.core/binding (walk-literal-binding-form (first params) (rest params) func ctx)
      clojure.core/reify   (walk-defprotocol-form form func ctx)
      ;; TODO proxy

      (if-let [tlw (top-level-walker-fn dispatch-form)]
        (tlw form func ctx)
        (walk-body params func ctx)))))

(defn walk-with-scope
  "walk an AST, calling (func context form) for every terminal/leaf form,
  collecting return values in a FLAT sequence, omitting nils"
  [{:keys [type text contents delim] :as form} func ^Context original-ctx]
  (try
    (let [ctx (assoc original-ctx :root?
                     (and (:root? original-ctx)
                          (or (= type :reader-conditional)
                              (= type :reader-conditional-splicing))))]
      (cond
        (= type :lambda)
        (let [lambda-ctx (-> ctx
                             (update :scope merge reader-lambda-scope)
                             (assoc :root? false))]
          ;; if you do #(let ...) we need to do walk-binding-form-here
          (walk-binding-form text func lambda-ctx))

        (nil? contents)
        (write! (func ctx form))

        (= type :syntax-quote)
        (walk-body contents func (update ctx :quote-depth inc))

        (or (= type :unquote) (= type :unquote-splicing))
        (walk-body contents func (update ctx :quote-depth dec))

        :else (walk-binding-form form func ctx)))
    (catch Exception e (throw (Exception. (str "walking " (parse/render form)) e)))))

(defn numeric?
  [text]
  (some? (re-find #"^[-+]?\d+(\.\d+)?M?$" text)))

(defn form->real-symbol
  [context {:keys [text] :as form}]
  (when (and (= :symbol (:type form))
             (not (or (contains? #{"true" "false" "nil"} text)
                      ;; hacks - consider syntax-quoted symbols as "real" only within macros
                      (and (pos? (:quote-depth context)) (nil? (get (:scope context) (symbol "&env"))))
                      (and (pos? (:quote-depth context)) (.endsWith text "#"))
                      (.startsWith text ".")
                      (.startsWith text "js/")
                      (numeric? text))))
    (symbol text)))

(defn form->free-symbol
  "Walking calls your fn on every leaf form, use this fn to get free symbols only"
  [context {:keys [text] :as form}]
  (when-let [sym (form->real-symbol context form)]
    (when (not (contains? (:scope context) sym))
      sym)))
