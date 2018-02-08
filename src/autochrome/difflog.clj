(ns autochrome.difflog
  (:require [clojure.string :as string]
            [autochrome.components :as comp]
            [autochrome.diff :as diff]
            [autochrome.page :as page]
            [autochrome.parse :as parse]
            [om.dom :as dom]))

(defn diff2
  [ann a b]
  (comp/panes
   {}
   (some->> a list (page/diff-pane "the absolute" ann))
   (some->> b list (page/diff-pane "worst hack" ann))))

(defn diff-log
  [aroot broot]
  (let [goalstate (diff/dforms aroot broot)]
    (println 'explored (count @diff/explored-states) 'states)
    (for [c @diff/explored-states
          :let [idhc (System/identityHashCode c)
                info (get @diff/state-info idhc)]]
      (let [shead (first (.-source c))
            thead (first (.-target c))]
        (dom/div
         {:id (str idhc)}
         (comp/heading
          (dom/span
           {}
           (str
            (if (identical? c goalstate) "goal! " "")
            "(" (string/join " " (map name (:attrib info))) ")"
            " [" (System/identityHashCode c) "]"
            " -" (count (.-deleted c))
            ",+" (count (.-added c))
            " cost " (.-cost c)
            "/" (- (.-cost c) (max (.-sremain c) (.-tremain c)))
            " remain " (.-sremain c)
            "/" (.-tremain c)
            (if (nil? shead) " (nil S)" "")
            (if (nil? thead) " (nil T)" ""))
           (dom/span {} " (" (Integer/toHexString idhc) " from "
                     (dom/a {:href (str "#" (:pred info))} (some-> (:pred info) Integer/toHexString))
                     ")")))
         (diff2
          (doto (diff/diffstate->annotations c)
            (.put shead :shead)
            (.put thead :thead))
          aroot
          broot)
         (comp/spacer))))))

(defn write-difflog
  [outdir title astr bstr]
  (let [a (parse/parse-one astr)
        b (parse/parse-one bstr)]
    (diff/dforms a b)
    (spit (str outdir "/" title ".html")
      (page/page
        title
        (comp/root {}
          (diff-log a b))))))

(comment
  (write-difflog
    "/tmp/a"
    "difflog2"
    "['[:a :b :c :d :e]]"
    "[(count '[:a :b :c :d :e])]"))
