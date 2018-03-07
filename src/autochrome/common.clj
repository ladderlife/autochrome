(ns autochrome.common)

(def ^:const special-forms
  ["def" "do" "loop" "if" "new" "fn" "try" "catch" "throw" "finally"
   "recur" "quote" "set!"])

(def special-form? (set special-forms))

(def open->closed
  {\( \)
   \[ \]
   \{ \}
   "#{" \}})

(def closed->open
  {\) \(
   \] \[
   \} \{})

(defn decoration?
  [{:keys [type]}]
  (or (= type :deref)
      (= type :syntax-quote)
      (= type :unquote)
      (= type :unquote-splicing)
      (= type :data-reader)
      (= type :meta)))

(defn ->decorator
  [{:keys [type] :as arg}]
  (case type
    :deref "@"
    :syntax-quote "`"
    :unquote "~"
    :unquote-splicing "~@"
    (throw (ex-info "what"  {:arg arg}))))

(def core-scope
  (delay
   (->> (ns-publics 'clojure.core)
        (keys)
        (map (fn [s] [s (symbol (name 'clojure.core) (name s))]))
        ;; def is a special form
        (into
         (->> (mapv symbol special-forms)
              (map (fn [s] [s (symbol (name 'clojure.core) (name s))]))
              (into {}))))))

(defn clojure-core-scope
  []
  @core-scope)
