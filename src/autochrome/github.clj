(ns autochrome.github
  (:require [clj-http.client :as http]
            [clojure.java.shell :as sh]
            [clojure.string :as string]
            [clojure.pprint :as pprint]))

(def ^:dynamic token "burbongleroy:00821ab66c37909e0d9ad5778ab3eb9a365ce53b")

(defn pr-request-params
  [owner repo num]
  (let [url (format "https://api.github.com/repos/%s/%s/pulls/%s" owner repo num)]
    {:method :get
     :url url
     ;; :basic-auth token
     :content-type :json}))

(defn pr-diffinfo
  [owner repo num]
  (let [params (pr-request-params owner repo num)]
    (try
      (let [info (-> params (assoc :as :json) http/request :body)
            diff (-> params
                   (assoc :accept "application/vnd.github.VERSION.diff")
                   http/request :body)]
        {:head (-> info :head :sha)
         :base (-> info :base :sha)
         :diff diff})
      (catch Exception e
        (throw (Exception. (str "fetching  " (pr-str (:url params))) e))))))

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

(defn diff->patches
  [diff]
  (let [lines (string/split-lines diff)
        put-line (fn [c k l] (update c k conj (.substring l 1)))
        line->path #(-> (second (.split % " "))
                        (strip-prefix "a/")
                        (strip-prefix "b/"))
        result (volatile! (transient []))
        default-ctx {:new [] :old []}]
    (loop [context default-ctx
           [line & lines] lines]
      (if-not line
        (persistent! (vswap! result conj! context))
        (cond
          (.startsWith line "diff --git")
          (do (when (:hunk context)
                (vswap! result conj! context))
              (recur default-ctx lines))

          (.startsWith line "---")
          (recur (assoc context :old-path (line->path line)) lines)

          (.startsWith line "+++")
          (recur (assoc context :new-path (line->path line)) lines)

          (.startsWith line "@@")
          (do (when (:hunk context)
                (vswap! result conj! context))
              (recur
               (merge context (assoc default-ctx :hunk (parse-hunk-spec line)))
               lines))

          (.startsWith line "+")
          (recur (put-line context :new line) lines)

          (.startsWith line "-")
          (recur (put-line context :old line) lines)

          :else
          (if-let [{:keys [old-lines new-lines]} (:hunk context)]
            (let [nnew (count (:new context))
                  nold (count (:old context))]
              (if (and (= nnew new-lines) (= nold old-lines))
                (do (vswap! result conj! context)
                    (recur (dissoc context :hunk) lines))
                (recur (-> context
                           (put-line :new line)
                           (put-line :old line))
                       lines)))
            (recur context lines)))))))

(defn apply-patches
  [old-text patches]
  (if (nil? old-text)
    ;; damn newline at end of file....
    (string/join "\n" (conj (:new (first patches)) ""))
    (let [lines (.split old-text "\n")
          line->patch (into {} (map (juxt (comp :old-start :hunk) identity) patches))
          sb (StringBuilder.)]
      (loop [idx 0]
        (if-not (< idx (count lines))
          (.toString sb)
          (let [linenum (inc idx)]
            (if-let [patch (line->patch linenum)]
              (do (doseq [line (:new patch)]
                    (.append sb line)
                    (.append sb "\n"))
                  (recur (+ idx (:old-lines (:hunk patch)))))
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
          ;; :basic-auth token
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

(defn slurp-blob-from-local-git
  [sha]
  (:out (sh/sh "git" "cat-file" "blob" sha)))

(defn ->changed-files
  [rawdiff slurp-old-blob-fn]
  (let [all-patches (diff->patches rawdiff)
        old-path->text
        (into {}
              (for [old-path (set (map :old-path all-patches))
                    :when (not= "/dev/null" old-path)]
                [old-path (future (slurp-old-blob-fn old-path))]))]
    (for [[new-path patches] (group-by :new-path all-patches)]
      (let [old-path (:old-path (first patches)) ; old-path is the same for all patches
            old-text (some-> old-path old-path->text deref)]
        (cond-> {:new-path new-path :new-text (apply-patches old-text patches)}
          old-path (assoc :old-path old-path)
          old-text (assoc :old-text old-text))))))

(defn pull-request-diff
  [owner repo num]
  (let [{:keys [base diff]} (pr-diffinfo owner repo num)]
    (->changed-files diff #(slurp-blob-from-github owner repo base %))))

(defn local-diff
  [oldref newref]
  (let [old-tree (ls-tree oldref)
        rawdiff (:out (sh/sh "git" "diff" oldref newref))]
    (->changed-files rawdiff #(slurp-blob-from-local-git (get old-tree %)))))

(defn local-merge-base-diff
  "this is what github shows you in the PR diff"
  ([] (local-merge-base-diff "HEAD"))
  ([ref]
   (local-diff
    (.trim (:out (sh/sh "git" "merge-base" ref "origin/master")))
    ref)))

