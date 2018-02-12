(ns autochrome.readme
  (:refer-clojure :exclude [comment])
  (:require [autochrome.annotation :as annotation]
            [autochrome.components :as comp]
            [autochrome.diff :as diff]
            [autochrome.difflog :as difflog]
            [autochrome.page :as page]
            [autochrome.parse :as parse]
            [autochrome.styles :as styles]
            [garden.core :refer [css]]
            [hiccup.page :as hp]
            [om.dom :as dom]))
(def readme-styles
  (css
   [:p :li :.caption
    {:font-family "sans-serif"
     :text-align "left"
     :font-size "18px"}]
   [:.caption {:width "57em"}]
   [:p {:text-indent "2em"}]
   [:.diffpane {:width "unset"}]
   [:.textcontainer {:width "50%"
                     :color "white"}]
   [:.title {:font-size "32px"}]
   [:.sectiontitle {:font-size "24px"
                    :text-decoration "underline"}]
   [:.insetcontainer {:display "flex"}]
   [:.insetcenter {:display "flex"
                   :justify-content "center"}]
   [:.inset {:border "2px solid"
             :border-color "#969896"
             :padding "10px"
             :display "flex"
             :margin "10px auto"}]
   [:.fixed {:font-family "monospace"
             :background-color "#111"}]))

(def page-title "Autochrome - Structural diffs for Clojure source code")

(defn readme-page
  [outfile root]
  (spit outfile
        (hp/html5
         [:head
          [:title page-title]
          [:style styles/styles]
          [:style readme-styles]]
         [:body (page/remove-react-stuff (dom/render-to-str root))])))

(defn inset
  [thing]
  (dom/div {:className "insetcontainer"}
           (dom/div {:className "inset"} thing)
           (dom/div {:style {:width "100%"}})))

(defn inset-center
  [thing]
  (dom/div {:className "insetcenter"}
           (dom/div {:className "inset"} thing)))

(dom/render-to-str (dom/div {:style [["min-width" "fit-content"] ["min-width" "-moz-fit-content"]]}))
;; => "<div style=\"min-width:fit-content asdf;\" data-reactroot=\"\" data-reactid=\"1\" data-react-checksum=\"-1031201701\"></div>"

(defn loginset
  [log]
  (inset log)
  (dom/div
   {:style {:margin-bottom "20px"}
    :className "insetcontainer"}
   (dom/div
    {:style [["min-width" "fit-content"]
             ["min-width" "-moz-fit-content"]]
     :className "inset"}
    log)
   (dom/div {:style {:width "100%"}})))

(defn term
  [& children]
  (inset-center (dom/pre {} children)))

(defn diff2
  [atext btext]
  (let [aroot (parse/parse-one atext)
        broot (parse/parse-one btext)]
    (difflog/diff2 (diff/diff-forms aroot broot) aroot broot)))

(defn code-inset
  [text]
  (let [parsed (parse/parse-one text)]
    (inset
     (comp/code
      (merge
       parsed
       {:linkbase "more hacks"
        :things (-> parsed
                    (annotation/attach
                     (annotation/syntax-highlighting parsed))
                    page/unnest)})))))

(defn p
  [& args]
  (dom/p {} args))

(defn caption
  [& args]
  (dom/div {:className "caption"} args))

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

(def highlight-example
  "(let [keyword :hello/world
      name (name keyword)
      [a b :as vec] (map inc [1 2 3])]
  (str (namespace keyword) name))")

(def wrap-example
  ["{:a :really
 :big :thing
 :these :entries
 :which :is
 :were :removed
 :very :annoying
 :to :read}"
   "(keys
   (merge
    {:a :really
     :big :thing
     :which :is
     :very :annoying
     :to :read}
    {:more :stuff}))"])

(def ws-example
  ["(def Y(fn[f]((fn[x](x,x))
(fn[x](f(fn[y]((x,x),y)))))))"
   "(def Y
  (fn [f]
    ((fn [x] (x x))
     (fn [x] (f (fn [y] ((x x) y)))))))"])

(defn comment
  [text]
  (dom/span {:className "comment"} text))

(defn fixed
  [text]
  (dom/span {:className "fixed"} text))

(readme-page
 "readme.html"
 (dom/div
  {}
  (dom/div {:className "filename-heading"}
           (dom/div {:className "title"} page-title))
  (dom/div
   {:className "textcontainer" :style {:margin "auto"}}
   (dom/div {:style {:margin-top "35px"}}
            (inset-center (diff2 (first example1) (second example1))))

   (dom/p
    {}
    (section
     "Abstract"
     (p (dom/a {:href "https://github.com/ladderlife/autochrome"} "Autochrome")
        " uses a full parse to highlight and structurally diff Clojure source code.  "
        "It aims to make the experience of reviewing Clojure code just as nice as writing it.  "
        "It takes the form of a command-line tool which writes diffs as HTML to stdout: ")
     (term "$ lein run <owner> <repo> <pull-request-id> "
           (comment " # generate diff for a GitHub pull request")

           "\n$ lein run <old-tree> <new-tree>"
           (comment "             # like git diff, run it from your repo directory")

           "\n$ lein run -o ..."
           (comment "                            # try to open the diff in a browser"))
     (p "If generated from GitHub, the line numbers in Clojure diffs link back to the PR.  "
        "Bold symbols link to documentation."))
    (section
     "Features"
     (p (dom/ul {}
                (dom/li {} "Scope-aware highlighting:"
                        (code-inset highlight-example))
                (dom/li {} "Structural diff which can cope with wrapping/stripping parens:"
                        (inset (diff2 (first wrap-example) (second wrap-example))))
                (dom/li {} "Naturally, whitespace is ignored completely: (h/t "
                        (dom/a {:href "http://blog.klipse.tech/lambda/2016/08/07/pure-y-combinator-clojure.html"}
                               "@viebel") ")"
                        (inset (diff2 (first ws-example) (second ws-example)))))))
    (section
     "Misfeatures"
     (p (dom/ul {}
                (dom/li {} "Symbols can only have one annotation.  ")
                (dom/li {} "Terrible for viewing non-clojure diffs.  ")
                (dom/li {} "Difficult to port to ClojureScript.  ")
                (dom/li {} "Uses its own custom clojure parser.")
                (dom/li {} "Occasionally gets strange ideas."))))
    (section
     "How it works"
     (p "Structural diffing is something I always wanted for Clojure.  "
        "When I saw "
        (dom/a {:href "http://thume.ca/2017/06/17/tree-diffing/"}
               "Tristan Hume's article about tree diffing")
        ", I was inspired to give it a shot myself using the same A* pathfinding technique he described.  "
        "In order to apply A* to the problem of tree diffing, you need to extend the concepts "
        "of location, cost, distance, and adjacency to tree diffs.  Location is clearly needed to know where you are, "
        "but in addition they need to be comparable, so you know not to bother when you already have a better "
        "path to the same place.  "
        "Cost is what makes some paths preferred over others.  For pathfinding on a road network, this would be "
        "the total distance traveled along the roads used.  By 'distance' I really mean the A* heuristic, "
        "which in the case of roads might be the straight-line distance to the destination.  "
        "Adjacency is what states are reachable from a particular state.  For roads you might say that intersections are "
        "the nodes and adjacency means there is a road connecting them.  "
        "In autochrome:"
        (dom/ul {}
                (dom/li {} "Location is a pair of pointers into the source and target lists, "
                        "plus the stack of previous locations.  Intuitively, the pointers represent a pair of "
                        "'cursors' over the tree structure.  Without the stack of previous locations, "
                        "comparison would break, since all locations at the end of two lists would be indistinguishable "
                        "from the goal (the end of both root lists)")
                (dom/li {} "Cost is the total size of all subtrees added and deleted, plus the number of subtree added and deleted.  "
                        "Subtree size is 1 for empty collections, character count for text nodes, and sum size of children for branch nodes.  "
                        "The number of subtrees changed is included in the cost so that the algorithm prefers deleting/adding entire"
                        "lists, rather than all their elements (since they have the same cost otherwise).")
                (dom/li {} "Distance is the maximum of the \"remaining\" tree sizes for the source and target tree.  "
                        "Whenever a subtree is accounted for by being added, deleted, or matched with an identical subtree, "
                        "its size is subtracted from the remaining size.")
                (dom/li {} "Adjacency is a bit complicated:"
                        (dom/ul {}
                                (dom/li {} "When the source and target cursors are over identical subtrees, we always advance both cursors.")
                                (dom/li {} "When the source cursor is not at the end of its list, we may advance it while keeping the same "
                                        "target cursor.  This corresponds to deleting a subtree from the source list.")
                                (dom/li {} "Likewise for the target cursor: we advance it and keep the source cursor, corresponding to a subtree addition.")
                                (dom/li {} "When both cursors are over matching collection types, we can move both cursors into the lists.  "
                                        "We also need to push the next location onto the stack.")
                                (dom/li {} "When both cursors are nil, it means we have reached the end of both lists, and we need to "
                                        "pop the next location in the parent sequences off the stack."))
                        (p "This is the basic version of adjacency that I started with.  However, when implemented this way, "
                           "the algorithm cannot match subtrees at different levels of nesting, since the cursors always move "
                           "up or down together.  "
                           "To handle changes in nesting, the cursors need to be allowed to move up and down independently, "
                           "like they are allowed to do within lists.  This means that instead of one stack of pairs of pointers, "
                           "we need a pair of stacks of pointers, one per cursor.  Then we need to add some state transitions: ")
                        (dom/ul {}
                                (dom/li {} "When only the source cursor is nil, pop the source stack only.")
                                (dom/li {} "Likewise for target cursor.")
                                (dom/li {} "When the source cursor is over a branch node, move it to the first child, "
                                        "and push the next position on the source stack.")
                                (dom/li {} "Likewise for target cursor."))
                        (p "Since there are quite a lot of branch nodes, this creates a ton of extra states for the algorithm to explore.  "
                           "So although it seems like the steps which move both cursors up/down would obsolete, since they can "
                           "be replicated with two single-cursor movements, they are needed so that performance is not terrible "
                           "on mostly identical subtrees (ie the common case).  It is also helpful to make single-cursor movement cost "
                           "more than two-cursor movement, so that we only try a single-cursor move after matched movement fails.  "
                           "The extra cost basically represents that we are adding or removing a set of parens, although they are not annotated. ")))))
    (let [a (parse/parse-one (first example1))
          b (parse/parse-one (second example1))
          logs (vec (difflog/diff-log a b))]
      (section
       "Worked Example"
       (p "I don't know about you, but I'm not the type who can absorb the essence of a complicated algorithm from a wall of text as seen above.  "
          "So let's look at a detailed log of all the states we explored when generating the example diff at the top of this page.  "
          "Note that these are all the states we " (dom/i {} "explore") " - some of them are never popped as the min-cost element of the PQ.  "
          "Here is our initial state:")
       (loginset (first logs))
       (caption "The source cursor is blue, and the target cursor is purple.  "
                "The header at the top shows information about the diff state:"
                (dom/ul {}
                        (dom/li {} (fixed "#000") "  is the step in which this state was added.  ")
                        (dom/li {} (fixed "(popped)") "  means this state was popped off the priority queue.  ")
                        (dom/li {} (fixed "-0/+0") "  is deletions/additions")
                        (dom/li {} (fixed "cost 0/-92") "  is the heuristic cost / real cost.  Meaningless for start node.")
                        (dom/li {} (fixed "remain 90/92") "  is the remaining cost of the source and target trees."))
                (caption "Note that real cost = heuristic cost - max(source remaining, target remaining).  "
                         "By heuristic cost, I mean the estimated total distance from the start to the goal, used as the key in the priority queue, "
                         "or " (dom/i {} "g(n) + h(n)") " in "
                         (dom/a {:href "https://en.wikipedia.org/wiki/A*_search_algorithm#Description"}
                                "typical A* notation.  ")))
       (caption "After we enter the main loop and pop the start state, we easily match the next two subtrees:")
       (loginset
        (comp/root {}
                   (nth logs 1) (comp/spacer)
                   (nth logs 2) (comp/spacer)
                   (nth logs 3)))

       (caption "Now we have our first mismatch.  We have a few options here:"
                (dom/ol {}
                        (dom/li {} "Delete blue subtree")
                        (dom/li {} "Add purple subtree")
                        (dom/li {} "Go into both subtrees")
                        (dom/li {} "Go into blue subtree only")
                        (dom/li {} "Go into purple subtree only")))
       (caption "We explore all of those options, but eventually we choose the last:")
       (loginset (nth logs 8))
       (caption "Then we add " (fixed "->"))
       (loginset (nth logs 12))
       (caption "Now, we enqueue #20, where we delete " (fixed "(println \"hello\")") ":")
       (loginset (nth logs 20))
       (caption "It turns out that since deleting " (fixed "(println \"hello\")") " has a relatively high cost, "
                "  we go through a bunch of other states before we finally pop #20.  "
                "But once we do, we can match the identical subtrees and get to here:")
       (loginset (nth logs 85))
       (caption "Here we are at the end of the source list (the defn body), "
                "so no cursor is drawn and it says (nil S) in the header.  "
                "From here we add " (fixed "(assoc :twice (+ x x))") ":")
       (loginset (nth logs 86))
       (caption "Now we have the completed diff, and we have (nil S) and (nil T), "
                "so all we need to do is pop out of the defn body and we're done!")
       (loginset (nth logs 88))))))))
