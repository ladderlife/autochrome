(ns autochrome.page
  (:require [clojure.string :as string]
            [hiccup.page :as hp]
            [autochrome.annotation :as annotation]
            [autochrome.components :as comp]
            [autochrome.diff :as diff]
            [autochrome.github :as github]
            [autochrome.styles :as styles]
            [autochrome.common :as clj-common]
            [autochrome.parse :as parse]
            [om.dom :as dom])
  (:import [java.security MessageDigest]
           [javax.xml.bind DatatypeConverter]))

(defn clojure-file?
  [s]
  (or (.endsWith s ".clj")
      (.endsWith s ".cljc")
      (.endsWith s ".cljs")))

(defn page
  [title root]
  (hp/html5
   [:head
    [:style styles/styles]
    [:title (str title)]]
   [:body (dom/render-to-str root)]))

(defn unnest
  [form]
  (let [acc (volatile! (transient []))
        sb (StringBuilder.)]
    (letfn [(emit [thing]
              (if (map? thing)
                (do (vswap! acc conj! (.toString sb))
                    (.setLength sb 0)
                    (vswap! acc conj! thing))
                (.append sb thing)))
            (go [{:keys [type text delim wscontents] :as form}]
                (cond
                  (and (= :coll type) (nil? (:annotation form)))
                  (do (emit delim)
                      (run! go wscontents)
                      (emit (clj-common/open->closed delim)))

                  (= :comment (:subtype form))
                  (emit form)

                  (= :ws type) (emit (:text form))
                  :else        (emit form)))]
      (go form)
      (persistent! (conj! @acc (.toString sb))))))

(defn md5sum
  [s]
  (-> (MessageDigest/getInstance "MD5")
      (.digest (.getBytes s))
      ;; https://stackoverflow.com/a/25758008
      DatatypeConverter/printHexBinary
      (.toLowerCase)))

(defn render-top-level-form
  [form]
  (comp/code (assoc form :things (unnest form))))

(defn diff-pane
  [linkbase ann contents]
  (for [f contents]
    (-> (annotation/attach f (doto (annotation/syntax-highlighting f) (.putAll ann)))
        (assoc :linkbase linkbase)
        render-top-level-form)))

(defn top-level-text
  [{:keys [contents]}]
  (string/join
   " "
   (keep identity
         (for [f contents]
           (case (:type f)
             :quote (parse/render f)
             (:keyword :symbol) (:text f)
             nil)))))

(defn two-file-diff
  [linkbase old new]
  (let [ann (diff/diff-forms (:root old) (:root new))
        bforms (:contents (:root new))
        aforms (:contents (:root old))
        adefs (group-by top-level-text aforms)
        bdefs (group-by top-level-text bforms)
        common? (set (filter #(= 1 (count (adefs %)) (count (bdefs %))) (keys bdefs)))
        matched-pairs
        (for [b bforms
              :when (common? (top-level-text b))
              :let [matched (first (get adefs (top-level-text b)))]
              :when (or (annotation/annotated? b ann)
                        (annotation/annotated? matched ann))]
          {:line (:start-line b)
           :forms [matched b]})
        unmatched-pairs
        (loop [result []
               [a :as as] aforms
               [b :as bs] bforms]
          (cond
            (and (empty? as) (empty? bs))
            (interpose (comp/spacer) result)

            (and (not (annotation/annotated? a ann))
                 (not (annotation/annotated? b ann)))
            (recur result (next as) (next bs))

            (common? (top-level-text a))
            (recur result (next as) bs)

            (common? (top-level-text b))
            (recur result as (next bs))

            :else
            (recur
             (conj result {:line (or (:start-line b) (:start-line a))
                           :forms [(and (annotation/annotated? a ann) a)
                                   (and (annotation/annotated? b ann) b)]})
             (next as)
             (next bs))))]
    (->> (concat matched-pairs unmatched-pairs)
         (sort-by :line)
         (map
          (fn [{[a b] :forms}]
            (comp/panes
             {}
             (some->> a list (diff-pane (str linkbase (md5sum (:path old)) "L") ann))
             (some->> b list (diff-pane (str linkbase (md5sum (:path new)) "R") ann))))))))

(defn diff-page
  [linkbase title changed-files]
  (page
   title
   (comp/root
    {}
    (->> changed-files
         (filter (comp clojure-file? :new-path))
         (mapcat
          (fn [{:keys [old-path old-text new-path new-text]}]
            [(comp/heading (cond
                             (= old-path "/dev/null") (str new-path " (new file)")
                             (not= old-path new-path) (str new-path " -> " new-path)
                             :else new-path))
             (binding [*out* *err*]
               (println new-path))
             (if-not old-text
               (comp/root
                {}
                (diff-pane (str linkbase (md5sum new-path) "R")
                           {}
                           (:contents (parse/parse new-text))))
               (two-file-diff
                linkbase
                {:path old-path :root (parse/parse old-text)}
                {:path new-path :root (parse/parse new-text)}))
             (comp/spacer)
             (comp/spacer)]))))))

(defn github-pr-diff-linkbase
  [owner repo num]
  (format "https://github.com/%s/%s/pull/%s/files#diff-" owner repo num))

(defn pull-request-diff
  "return full html text"
  [owner repo num]
  (diff-page
   (github-pr-diff-linkbase owner repo num)
   (str owner "/" repo " #" num)
   (github/pull-request-diff owner repo num)))

(defn merge-base-diff
  "return full html text"
  ([rev] (merge-base-diff "whatever" rev))
  ([linkbase rev]
   (diff-page
     linkbase
     (str "merge base diff " rev)
     (github/local-merge-base-diff rev))))


