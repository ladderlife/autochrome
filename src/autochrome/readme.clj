(ns autochrome.readme
  (:require [garden.core :refer [css]]
            [hiccup.page :as hp]
            [autochrome.styles :as styles]
            [om.dom :as dom]))

(def readme-styles
  (css
   [:p {:font-family "sans-serif"
        :color "white"
        :text-indent "5em"
        :text-align "left"}]
   [:.textcontainer {:width "70%"}]
   [:.title {:font-size "32px"}]))

(defn readme-page
  [outfile root]
  (spit outfile
        (hp/html5
         [:head
          [:style styles/styles]
          [:style readme-styles]]
         [:body (dom/render-to-str root)])))

(comment
  (readme-page
   "/tmp/readme.html"
   (dom/div
    {}
    (dom/div {:className "filename-heading"}
             (dom/div {:className "title"} "Title text goes here"))
    (dom/div
     {:className "textcontainer" :style {:margin "auto"}}
     (dom/p {} "this is my first paragraph")
     (dom/p {} "this is another paragraph")))))
