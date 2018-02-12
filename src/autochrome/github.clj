(ns autochrome.github
  (:require [clj-http.client :as http]
            [clojure.java.shell :as sh]
            [clojure.string :as string]))

(defn pr-request-params
  [owner repo num]
  (let [url (format "https://api.github.com/repos/%s/%s/pulls/%s" owner repo num)]
    {:method :get
     :url url
     :content-type :json}))

(defn pr-diffinfo
  [owner repo num]
  (let [params (pr-request-params owner repo num)]
    {:info (try (-> params
                    (assoc :as :json)
                    http/request :body)
                (catch Exception e
                  (throw (Exception. (str "getting info url=" (pr-str (:url params)))))))
     :diff (try (-> params
                    (assoc :accept "application/vnd.github.VERSION.diff")
                    http/request :body)
                (catch Exception e
                  (throw (Exception. "getting diff"))))}))

(defn parse-hunk-spec
  [hunk]
  (zipmap
   [:old-start :old-lines :new-start :new-lines]
   (map read-string     ; fine to read the numbers we just regex'd out
        (rest (re-find #"@@ -(\d+),(\d+) \+(\d+),(\d+) @@" hunk)))))

(defn strip-prefix
  [s pre]
  (if-not (.startsWith s pre)
    s
    (.substring s (count pre))))

(defn parse-diff
  [diff]
  (let [lines (string/split-lines diff)
        put-line (fn [c k l] (update c k conj (.substring l 1)))
        line->path #(-> (second (.split % " "))
                        (strip-prefix "a/")
                        (strip-prefix "b/"))
        hunks (volatile! (transient []))
        filechanges (volatile! (transient []))
        default-ctx {:new [] :old [] :start 0}]
    (loop [context default-ctx
           line-index 0]
      (let [line (get lines line-index)]
        (if-not line
          {:hunks (persistent! (vswap! hunks conj! context))
           :filechanges
           (persistent!
            (vswap! filechanges conj!
                    (assoc context :raw
                           (subvec lines (:start context) line-index))))}
          (cond
            (.startsWith line "diff --git")
            (do (when (:hunk context)
                  (vswap! hunks conj! context)
                  (vswap! filechanges conj!
                          (assoc context :raw
                                 (subvec lines (:start context) line-index))))
                (recur (assoc default-ctx :start line-index) (inc line-index)))

            (.startsWith line "---")
            (recur (assoc context :old-path (line->path line)) (inc line-index))

            (.startsWith line "+++")
            (recur (assoc context :new-path (line->path line)) (inc line-index))

            (.startsWith line "@@")
            (do (when (:hunk context)
                  (vswap! hunks conj! context))
                (recur
                 (merge context {:new [] :old [] :hunk (parse-hunk-spec line)})
                 (inc line-index)))

            (.startsWith line "+")
            (recur (put-line context :new line) (inc line-index))

            (.startsWith line "-")
            (recur (put-line context :old line) (inc line-index))

            :else
            (if-let [{:keys [old-lines new-lines]} (:hunk context)]
              (let [nnew (count (:new context))
                    nold (count (:old context))]
                (if (and (= nnew new-lines) (= nold old-lines))
                  (do (vswap! hunks conj! context)
                      (recur (dissoc context :hunk) (inc line-index)))
                  (recur (-> context
                             (put-line :new line)
                             (put-line :old line))
                         (inc line-index))))
              (recur context (inc line-index)))))))))

;; need to apply patches in reverse because I don't know how to get the
;; old text to diff from using the github api
(defn reverse-apply-patches
  [new-text patches]
  (if-not new-text
    (string/join "\n" (conj (map :old patches) ""))
    (let [lines (.split new-text "\n")
          line->patch (into {} (map (juxt (comp :new-start :hunk) identity) patches))
          sb (StringBuilder.)]
      (loop [idx 0]
        (if-not (< idx (count lines))
          (.toString sb)
          (let [linenum (inc idx)]
            (if-let [{:keys [hunk] :as patch} (line->patch linenum)]
              (do
                (doseq [line (:old patch)]
                  (.append sb line)
                  (.append sb "\n"))
                (recur (dec (+ (:new-start hunk) (:new-lines hunk)))))
              (do (.append sb (nth lines idx))
                  (.append sb "\n")
                  (recur (inc idx))))))))))

(defn slurp-blob-from-github
  [owner repo tree path]
  (let [url (format "https://raw.githubusercontent.com/%s/%s/%s/%s" owner repo tree path)]
    (try
      (:body
       (http/request
        {:method :get
         :url url
         :content-type :json
         :accept "application/vnd.github.VERSION.raw"}))
      (catch Exception e
        (throw (Exception. (str "slurping " (pr-str url)) e))))))

;; local git stuff
(defn ls-tree
  [rev]
  (reduce
   (fn [m line]
     (let [sp   (.split line "\\s")
           ;; [mode type sha path]
           sha  (aget sp 2)
           path (aget sp 3)]
       (cond-> m (.contains path ".clj") (assoc path sha))))
   {}
   (-> (sh/sh "git" "ls-tree" "-r" rev) :out (.split "\n"))))

(defn ->changed-files
  [rawdiff slurp-new-blob-fn]
  (let [{:keys [hunks filechanges]} (parse-diff rawdiff)
        new-path->text
        (into {}
              (for [new-path (set (map :new-path hunks))]
                [new-path (future (slurp-new-blob-fn new-path))]))
        new-path->rawdiff (group-by :new-path filechanges)
        old-path->rawdiff (group-by :old-path filechanges)]
    (concat
     (for [[new-path patches] (group-by :new-path hunks)
           :when (not= "/dev/null" new-path)]
       (let [new-path (:new-path (first patches))
             new-text @(new-path->text new-path)
             old-path (:old-path (first patches))
             old-text (reverse-apply-patches new-text patches)]
         (cond-> {:new-path new-path :new-text new-text
                  :rawdiff (-> new-path new-path->rawdiff first :raw)}
           old-path (assoc :old-path old-path)
           old-text (assoc :old-text old-text))))
     (for [[old-path patches] (->> hunks
                                   (filter #(= "/dev/null" (:new-path %)))
                                   (group-by :old-path))]
       {:old-path old-path
        :old-text (string/join "\n" (conj (mapcat :old patches) ""))
        :new-path "/dev/null"
        :new-text ""
        :rawdiff (-> old-path old-path->rawdiff first :raw)}))))

(defn pull-request-diff
  [owner repo num]
  (let [{:keys [diff info]} (pr-diffinfo owner repo num)
        src (-> info :head :repo)]
    (->changed-files
     diff
     #(slurp-blob-from-github (-> src :owner :login) (:name src) (-> info :head :sha) %))))

(defn slurp-blob-from-local-git
  [sha]
  (let [result (sh/sh "git" "cat-file" "blob" sha)]
    (when (= 0 (:exit result))
      (:out result))))

(defn local-diff
  [oldref newref]
  (let [new-tree (ls-tree newref)
        rawdiff (:out (sh/sh "git" "diff" oldref newref))]
    (->changed-files
     rawdiff
     #(when-let [sha (get new-tree %)]
        (slurp-blob-from-local-git sha)))))

(defn merge-base
  [head base]
  (.trim (:out (sh/sh "git" "merge-base" head base))))
