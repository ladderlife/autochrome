(ns autochrome.styles
  (:require [garden.core :refer [css]]
            [garden.selectors :as gs]))

(def code-fonts
  "some nice programming fonts, ordered by how much I like them"
  "\"Iosevka Term\", \"Fira Code\", Inconsolata, Menlo, Monaco, Consolas, monospace")

(def styles
  (css
   [:body {:background "#000000"
           :font-family code-fonts
           :font-size "16px"}]
   [:a {:text-decoration :none
        :font-weight :bolder}]
   [:code :pre
    {:color "#ffffff"
     :background "#000000"
     ;; pre does not inherit font from body
     :font-family code-fonts
     :white-space :pre
     :word-spacing :normal
     :word-break :normal
     :word-wrap :normal
     :line-height 1.24
     :hyphens :none}]
   [:code :a {:text-decoration :none
              :font-weight :bolder
              :color :inherit}]
   [:pre {:margin "0px"
          :overflow :auto
          :border-radius "0.3em"
          :font-weight :normal
          :overflow-y :hidden}]
   [:.gutter {:text-align :right
              :padding-right "4px"
              :padding-left "2px"}]
   [:.code-card {:margin-buttom "16px"
                 :padding "6px 6px 0px 6px"
                 :color "#ae81ff"
                 :font-weight :bold}]
   [:.code-card-heading {:background "linear-gradient(#632697, #000)"
                         :font-size "22px"
                         :display :flex
                         :justify-content :space-between}]
   [:.filename-heading {:background "linear-gradient(#632697, #000)"
                        :font-size "22px"
                        :color :white}]
   [:.spacer {:margin-bottom "30px"}]
   [:.code-card-heading-extra {:display :flex}]
   [:.container {:display :flex
                 :flex-direction :row}]
   [:.usages {:font-size "16px"
              :margin :auto}]
   [:.top-comment {:margin-bottom "17px"}]
   [:.ns-ref {:color "#f0c674"}]
   [:.keyword {:color "#8abeb7"}]
   [:.var-ref {:color "#81a2be"}]
   [:.clojure-core {:font-weight :bolder :color "#81a2be"}]
   [:.macro {:color "#b294bb"}]
   [:.punctuation {:color "#a6a6a0"}]
   [:.ns-ref {:color "#f0c674"}]
   [:.string {:color "#b5bd68"}]
   [:.keyword {:color "#70c0b1"}]
   [:.meta-keyword {:color "#ea731c"}]
   [:.java-class {:color "#de9f25"}]
   [:.punctuation {:color "#a6a6a0"}]
   [:.highlight {:background-color "#5d007a"}]
   [:.unparsed {:background-color "#ff0000"}]
   [:.added {:background-color "#225d2d"}]
   [:.deleted {:background-color "rgba(200, 38, 38, 0.81)"}]
   [:.deleted [:.deleted {:background-color "unset"}]]
   [:.added [:.added {:background-color "unset"}]]
   [:.first-seen {:text-decoration :underline}]
   [:.comment {:color "#969896" :font-style :italic :font-weight :normal}]
   [:.diffpane {:width "50%"}]

   ;; for difflog

   [:.shead {:background-color "#1224b1" :border-radius "10px"}]
   [:.thead {:background-color "#5d007a" :border-radius "10px"}]))
