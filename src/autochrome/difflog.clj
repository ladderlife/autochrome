(ns autochrome.difflog
  (:require [clojure.string :as string]
            [autochrome.components :as comp]
            [autochrome.annotation :as ann]
            [autochrome.diff :as diff]
            [autochrome.page :as page]
            [autochrome.parse :as parse]
            [om.dom :as dom]))

(defn diff2
  [ann a b]
  (comp/panes
   {}
   (some->> a list (page/diff-pane "the absolute"
                                   (doto (ann/syntax-highlighting a)
                                     (.putAll ann))))
   (some->> b list (page/diff-pane "worst hack"
                                   (doto (ann/syntax-highlighting a)
                                     (.putAll ann))))))



(defn diff-log
  [aroot broot]
  (let [goalstate (diff/dforms aroot broot)
        maxdigits (count (str (count @diff/explored-states)))]
    (println 'explored (count @diff/explored-states) 'states)
    (for [index (range (count @diff/explored-states))
          :let [c (nth @diff/explored-states index)
                idhc (System/identityHashCode c)
                info (get @diff/state-info idhc)]]
      (let [shead (first (.-source c))
            thead (first (.-target c))]
        (dom/div
         {:id (str idhc)}
         (comp/heading
          (dom/span
           {}
           (str
            (format (str "#%0" maxdigits "d ") index)
            (if (identical? c goalstate) "goal! " "")
            "(" (string/join " " (map name (:attrib info))) ")"
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
         #_(comp/spacer))))))

(defn write-difflog
  [outdir title astr bstr]
  (let [a (parse/parse-one astr)
        b (parse/parse-one bstr)]
    #_(diff/dforms a b)
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
