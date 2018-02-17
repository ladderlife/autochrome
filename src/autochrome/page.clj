(ns autochrome.page
  (:require [autochrome.annotation :as annotation]
            [autochrome.common :as clj-common]
            [autochrome.components :as comp]
            [autochrome.diff :as diff]
            [autochrome.github :as github]
            [autochrome.parse :as parse]
            [autochrome.styles :as styles]
            [clojure.string :as string]
            [hiccup.page :as hp]
            [om.dom :as dom])
  (:import [java.security MessageDigest]
           [javax.xml.bind DatatypeConverter]
           [java.util IdentityHashMap]))

(defn clojure-file?
  [s]
  (and s
       (or (.endsWith s ".clj")
           (.endsWith s ".cljc")
           (.endsWith s ".cljs"))))

(defn remove-react-stuff
  "no need for this until we have a client-side..."
  [html]
  (-> html
      (.replaceAll "<!--.*?-->" "")
      (.replaceAll " data-reactid=\"[0-9]+\"" "")))

(defn page
  [title root]
  (hp/html5
   [:head
    [:style styles/styles]
    [:title (str title)]]
   [:body (remove-react-stuff (dom/render-to-str root))]))

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
  (let [aforms (:contents (:root old))
        bforms (:contents (:root new))
        adefs (group-by top-level-text aforms)
        bdefs (group-by top-level-text bforms)
        common? (set (filter #(= 1 (count (adefs %)) (count (bdefs %))) (keys bdefs)))
        matched-pairs
        (for [b bforms
              :when (common? (top-level-text b))
              :let [matched (first (get adefs (top-level-text b)))]]
          {:line (:start-line b)
           :forms [matched b]})
        unmatched-pairs
        (loop [result []
               [a :as as] aforms
               [b :as bs] bforms]
          (cond
            (and (empty? as) (empty? bs))
            result

            (common? (top-level-text a))
            (recur result (next as) bs)

            (common? (top-level-text b))
            (recur result as (next bs))

            :else
            (recur
             (conj result {:line (or (:start-line b) (:start-line a))
                           :forms [a b]})
             (next as)
             (next bs))))
        ann2
        (fn [a b]
          (cond
            (nil? a) (doto (IdentityHashMap.) (.put b :added))
            (nil? b) (doto (IdentityHashMap.) (.put a :deleted))
            :else (diff/diff-forms a b)))
        diff2
        (fn [a b]
          (let [ann (ann2 a b)]
            (when-not (.isEmpty ann)
              (comp/panes
               {}
               (some->> a list (diff-pane (str linkbase (md5sum (:path old)) "L") ann))
               (some->> b list (diff-pane (str linkbase (md5sum (:path new)) "R") ann))))))]
    (->> (concat matched-pairs unmatched-pairs)
         (sort-by :line)
         (keep
          (fn [{[a b] :forms}]
            (let [empty-form {:type :root :contents []}]
              (diff2 a b))))
         (interpose (comp/spacer)))))

(defn delete-everything
  [root]
  (assoc root :contents (map #(assoc % :annotation :deleted) (:contents root))))

;; don't restrict width to 50% when there is no other file to display
(defn one-file-diff
  [linkbase path lr root]
  (comp/root {} (diff-pane (str linkbase (md5sum path) lr) {} (:contents root))))

(defn patch-heading
  [{:keys [old-path old-text new-path new-text]}]
  (comp/heading
   (cond
     (= old-path "/dev/null") (str new-path " (new file)")
     (= new-path "/dev/null") (str old-path " (deleted)")
     (not= old-path new-path) (str new-path " -> " new-path)
     :else new-path)))

(defn clojure-diff
  [linkbase {:keys [old-path old-text new-path new-text] :as patch}]
  (cond
    (= old-path "/dev/null")
    (one-file-diff linkbase new-path "R" (parse/parse new-text))

    (= new-path "/dev/null")
    (one-file-diff linkbase old-path "L"
                   (-> old-text parse/parse delete-everything))

    :else
    (two-file-diff
     linkbase
     {:path old-path :root (parse/parse old-text)}
     {:path new-path :root (parse/parse new-text)})))

(defn raw-diff
  [linkbase {:keys [rawdiff]}]
  (->> (for [line (drop 4 rawdiff)]
         (cond->> (.substring line 1)
           (.startsWith line "+") (dom/span {:className "added"})
           (.startsWith line "-") (dom/span {:className "deleted"})))
       (interpose "\n")
       (dom/pre {})))

(defn diff-page
  [linkbase title changed-files]
  (println (count changed-files) "changed files")
  (->> changed-files
       (mapcat
        (fn [{:keys [old-path old-text new-path new-text] :as patch}]
          [(patch-heading patch)
           (if (or (clojure-file? new-path)
                   (and (= "/dev/null" new-path)
                        (clojure-file? old-path)))
             (clojure-diff linkbase patch)
             (raw-diff linkbase patch))
           (comp/spacer)
           (comp/spacer)]))
       (comp/root {})
       (page title)))

(defn github-pr-diff-linkbase
  [owner repo num]
  (format "https://github.com/%s/%s/pull/%s/files#diff-" owner repo num))

;; these return the html as string
(defn pull-request-diff
  [owner repo num]
  (diff-page
   (github-pr-diff-linkbase owner repo num)
   (str owner "/" repo " #" num)
   (github/pull-request-diff owner repo num)))

(defn local-diff
  [a b]
  (diff-page
   "local"
   (str a "..." b)
   (github/local-diff a b)))
