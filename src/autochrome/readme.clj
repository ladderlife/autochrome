(ns autochrome.readme
  (:require [garden.core :refer [css]]
            [hiccup.page :as hp]
            [autochrome.parse :as parse]
            [autochrome.diff :as diff]
            [autochrome.difflog :as difflog]
            [autochrome.styles :as styles]
            [om.dom :as dom]))

(def readme-styles
  (css
   [:p {:font-family "sans-serif"
        :text-indent "2em"
        :text-align "left"
        :font-size "18px"}]
   [:.diffpane {:width "unset"}]
   [:.textcontainer {:width "64%"
                     :color "white"}]
   [:.title {:font-size "32px"}]
   [:.sectiontitle {:font-size "24px"}]
   [:.insetcontainer {:display "flex"
                       :justify-content "center"}]
   [:.inset {:border "2px solid"
                  :border-color "#969896"
                  :padding "10px"
                  :display "flex"
                  :margin "10px auto"}]))

(defn readme-page
  [outfile root]
  (spit outfile
        (hp/html5
         [:head
          [:style styles/styles]
          [:style readme-styles]]
         [:body (dom/render-to-str root)])))

(defn inset
  [thing]
  (dom/div
   {:className "insetcontainer"}
   (dom/div {:className "inset"} thing)))

(defn term
  [& children]
  (inset (dom/pre {} children)))

(defn diff-inset
  [atext btext]
  (let [aroot (parse/parse-one atext)
        broot (parse/parse-one btext)]
    (inset (difflog/diff2 (diff/diff-forms aroot broot) aroot broot))))

(defn p
  [& args]
  (dom/p {} args))

(defn section
  [title & children]
  (dom/div
   {}
   (dom/div {:className "sectiontitle"} title)
   children))

(def example1
  ["(defn example
  [x]
  (println \"hello!\")
  {:more (inc x)
   :less (dec x)})"

   "(defn example
  [x]
  (-> {:more (inc x)
       :less (dec x)}
      (assoc :twice (+ x x))))"])


(comment
  (difflog/write-difflog
   "/tmp/"
   "difflog2"
   (first example1) (second example1)))

(readme-page
 "/tmp/readme.html"
 (dom/div
  {}
  (dom/div {:className "filename-heading"}
           (dom/div {:className "title"}
                    "Autochrome - Structural diffs for Clojure source code"))
  (dom/div
   {:className "textcontainer" :style {:margin "auto"}}
   (dom/div {:style {:margin-top "35px"}}
    (diff-inset (first example1) (second example1)))
     
   (dom/p
    {}
    (section
     "Abstract"
     (p "Autochrome uses a full parse to highlight and structurally diff Clojure source code.  "
        "It aims to make the experience of reviewing Clojure code just as nice as writing it.  "
        "It takes the form of a command-line tool which writes diffs as HTML to stdout: ")
     (term "$ lein run <owner> <repo> <pull-request-id> "
           (dom/span {:className "comment"} " # generate diff for a GitHub pull request")
           
           "\n$ lein run <old-tree> <new-tree>"
           (dom/span {:className "comment"} "             # like git diff, run it from your repo directory")

           "\n$ lein run -o ..."
           (dom/span {:className "comment"}
                     "                            # try to open the diff in a browser"))
     (p "Only Clojure/ClojureScript sources are included in the diff.  "
        "If generated from GitHub, the line numbers link back to the PR."))
    (section
     "Motivation"
     (p "Since I started using Clojure last year, I've come to really appreciate the language, and its tooling.  "
        "REPL-driven development and structural editing make me feel like"
        (dom/a {:href "https://mitpress.mit.edu/sicp/full-text/book/cover.jpg"}
               " the wizard guy on the cover of SICP.   ")
        "But once the code is pushed to GitHub, it loses all of its magical properties.  "))))))
