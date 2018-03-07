(ns autochrome.difflog
  (:require [autochrome.annotation :as ann]
            [autochrome.components :as comp]
            [autochrome.diff :as diff]
            [autochrome.page :as page]
            [autochrome.parse :as parse]
            [om.dom :as dom]))

(defn diff2
  [ann a b]
  (comp/panes
   {}
   (when a
     (page/render-top-level-form
       (ann/attach a (doto (ann/syntax-highlighting a) (.putAll ann)))))
   (when b
     (page/render-top-level-form
       (ann/attach b (doto (ann/syntax-highlighting b) (.putAll ann)))))))

(defn diff-log
  [aroot broots]
  (let [goalstate (diff/dforms aroot broots)
        maxdigits (count (str (count @diff/explored-states)))]
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
           {:style {:font-size "16px"}}
           (str
            (format (str "#%0" maxdigits "d ") index)
            (if (identical? c goalstate) "goal! " "")
             ; "(" (string/join " " (map name (:attrib info))) ")"
            " -" (count (filter (comp #{:deleted :parens-deleted} second) (.-changes c)))
            ",+" (count (filter (comp #{:added :parens-added} second) (.-changes c)))
            " cost " (.-cost c)
            ;; "/" (- (.-cost c) (max (.-sremain c) (.-tremain c)))
            ; " remain " (.-sremain c)
            ;"/" (.-tremain c)
            (if (nil? shead) " (nil S)" "")
            (if (nil? thead) " (nil T)" ""))
           #_(dom/span {} " (" (Integer/toHexString idhc) " from "
                       (dom/a {:href (str "#" (:pred info))} (some-> (:pred info) Integer/toHexString))
                       ")")))
         (diff2
          (doto (diff/diffstate->annotations c)
            (.put shead :shead)
            (.put thead :thead))
          aroot
          (.-origtarget c))
         #_(comp/spacer))))))


(defn write-difflog
  [title astr bstrs]
  (let [a (parse/parse-one astr)
        bs (map parse/parse-one bstrs)]
    (spit (str title ".html")
          (page/page
            title
            (comp/root {}
                       (diff-log a bs))))))

(comment
  (write-difflog
   "difflog2"
   "#:: {:foo :bar}"
   ["#::some-ns{:foo :bar}"]))
