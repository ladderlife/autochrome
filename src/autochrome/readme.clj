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
   [:body {:color "white"}]
   [:p :.caption
    {:font-family "sans-serif"
     :text-align "left"
     :font-size "18px"}]
   [:.caption {:font-size "14px"}]
   [:p {:text-indent "2em"}]
   [:.diffpane {:width "unset"}]
   [:.text {:font-family "sans-serif"}]
   [:.textcontainer {:width "67%"
                     :font-size "18px"}]
   [:.title {:font-size "32px"}]
   [:.sectiontitle {:font-size "24px"
                    :text-decoration "underline"}]
   [:.insetcontainer {:display "flex"
                      :justify-content "space-between"}]
   [:.insetcenter {:display "flex"
                   :justify-content "center"}]
   [:.logside {:width "50%"}]
   [:.inset {:border       "2px solid"
             :border-color "#969896"
             :padding      "10px"
             :display      "flex"
             ;:margin "10px auto"
             }]
   [:.examplesection {:font-size "16px"
                      :width "70%"
                      :margin "auto"}]
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

(defn loginset
  [log & more]
  (inset log)
  (dom/div
    {:style {:margin-bottom "40px"}
     :className "insetcontainer"}
    (dom/div
      {:style [["min-width" "fit-content"]
               ["min-width" "-moz-fit-content"]]
       :className "inset"}
      log)
    (dom/div {})
    (dom/div {:className "logside"} more)))

(defn term
  [& children]
  (inset-center (dom/pre {} children)))

(defn diff2
  [atext btext]
  (let [aroot (parse/parse atext)
        broot (parse/parse btext)]
    (difflog/diff2 (diff/diff-forms aroot broot) aroot broot)))

(defn side-caption
  [& body]
  (dom/div {:style {:display "flex"
                    :height "100%"
                    :flex-direction "column"
                    :justify-content "center"}}
    (dom/div {})
    (dom/div {:className "caption"} body)))

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

(defn section*
  [props title & children]
  (dom/div
   props
   (dom/div {:className "sectiontitle"} title)
   children))

(defn section
  [title & children]
  (apply section* {} title children))

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
     (p (dom/a {:href "https://github.com/ladderlife/autochrome"} "Autochrome (repo here)")
        " uses a full parse to highlight and structurally diff Clojure source code.  "
        "It aims to make the experience of reviewing Clojure code just as nice as writing it.  "
        "It takes the form of a command-line tool which generates diffs as static HTML: ")
     (term
       "$ lein run " (dom/i {} "owner") " " (dom/i {} "repo") " " (dom/i {} "num") " -o diff.html"
       (comment "        # write a diff for a GitHub pull request")

       "\n$ lein run --token user:123abc "(dom/i {} "owner") " " (dom/i {} "repo") " " (dom/i {} "num")
       (comment " # use supplied auth token for github api")

       "\n$ lein run ... --open"
       (comment "                         # try to open the diff in a browser")

       "\n$ lein uberjar"
       (comment "                          # create a standalone jar in target/ directory")

       "\n$ java -jar autochrome.jar " (dom/i {} "old-tree") " " (dom/i {} "new-tree")
       (comment "  # run like git diff from your repo"))
     (p "If generated from GitHub, the line numbers in Clojure diffs link back to the PR.  "
        "Bold symbols link to documentation."))
    (section
     "Features"
     (p (dom/ul {:className "text"}
                (dom/li {:style {:margin-bottom "30px"}} "Scope-aware highlighting (no regular expressions):"
                        (code-inset highlight-example))
                (dom/li {:style {:margin-bottom "30px"}} "Structural diff which can cope with wrapping/stripping parens:"
                        (inset (diff2 (first wrap-example) (second wrap-example))))
                (dom/li {:style {:margin-bottom "30px"}} "Naturally, whitespace is ignored completely: (h/t "
                        (dom/a {:href "http://blog.klipse.tech/lambda/2016/08/07/pure-y-combinator-clojure.html"}
                               "@viebel") ")"
                        (inset (diff2 (first ws-example) (second ws-example)))))))
    (section
     "Misfeatures"
     (p (dom/ul {:className "text"}
                (dom/li {} "Symbols can only have one annotation.  (diff color overwrites highlight)")
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
        (dom/ul {:className "text"}
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
                           "The extra cost accounts for the fact that single-cursor movement corresponds to "
                           "adding or removing a set of parens."))))))
    (section
      "Worked Example"
      (p "I don't know about you, but I'm not the type who can absorb the essence of a complicated algorithm from a wall of text as seen above.  "
        "So let's look at a detailed log of all the states we popped from our A* priority queue while generating the example diff at the top of this page.  "
        "The states are numbered in the order in which they were explored, however I will only show the goal state and its predecessors,  "
        "starting from the initial state.")))
   (let [a (parse/parse (first example1))
         b (parse/parse (second example1))
         logs (vec (difflog/diff-log a b))]
     (dom/div {:className "examplesection"}
       (loginset (first logs)
         (side-caption "The source cursor is blue, and the target cursor is purple.  "
           "As you can see, we start with each cursor over its entire subtree.  "
           "The header at the top shows information about the diff state:"
           (dom/ul {}
             (dom/li {} (fixed "-0,+0") "  is the number of deletions and additions")
             (dom/li {} (fixed "cost 0/-92") "  is the heuristic cost / real cost.  Meaningless for start node.")
             (dom/li {} (fixed "remain 90/92") "  is the remaining cost of the source and target trees."))
           "Note that real cost = heuristic cost - max(source remaining, target remaining).  "
           "By heuristic cost, I mean the estimated total distance from the start to the goal, used as the key in the priority queue, "
           "or " (dom/i {} "g(n) + h(n)") " in "
           (dom/a {:href "https://en.wikipedia.org/wiki/A*_search_algorithm#Description"}
             "typical A* notation.  ")))
       (loginset
         (nth logs 3)
         (side-caption "After we enter the main loop and pop the start state, we can start exploring.  "
           "In this state we have matched the parentheses and descended into the defn body. "))
       (loginset
         (nth logs 6)
         (side-caption "We matched " (fixed "defn") " with " (fixed "defn") " and advanced both cursors. "
           "Now we can now match " (fixed "example") " with " (fixed "example") "."))
       (loginset
         (nth logs 7)
         (side-caption "Since matching is done with subtree hashes, we can match " (fixed "[x]")
           " without going into the vector at all."))
       (loginset
         (nth logs 8)
         (side-caption
           "Now we have our first mismatch.  We have a few options here:"
           (dom/ol {}
             (dom/li {} "Delete source (blue) subtree")
             (dom/li {} "Add target (purple) subtree")
             (dom/li {} "Go into both subtrees")
             (dom/li {} "Go into blue subtree only")
             (dom/li {} "Go into purple subtree only"))))
       (loginset
         (nth logs 13)
         (side-caption "We explore all of those options, but eventually we choose the last.  "
           "Since we moved the target cursor into a list while the source cursor stayed put, "
           "it follows that if we finish diffing, the parens which create that extra list must have been added, "
           "so we can go ahead and paint them green."))
       (loginset
         (nth logs 20)
         (side-caption "Add the " (fixed "->") "."))
       (loginset
         (nth logs 25)
         (side-caption "Now we enqueue this state, #25, where we delete " (fixed "(println \"hello\")") ".  "
           "Since it's a relatively large subtree, deleting it has a high cost, "
           "so we actually spend a lot of time going through other states before it is finally popped."))
       (loginset
         (nth logs 115)
         (side-caption "But once we do pop it, we can match the identical maps under the cursors.  "
           "Since the map was the last element in the source defn body, the source cursor has reached the "
           "end of its list, so there is nothing to highlight in blue and it says "
           (fixed "(nil S)") " in the header."))
       (loginset
         (nth logs 122)
         (side-caption "Add " (fixed "(assoc :twice (+ x x))") ".  This is the last element in the current "
           "target sequence, so now we have " (fixed "(nil S)") " and " (fixed "(nil T)") ".  "
           "At this point the source stack is " (fixed "[nil]") " and the target stack is " (fixed "[nil nil]") ", "
           "so we have to do two steps where we pop both and then pop target only, but the order doesn't matter."))
       (loginset
         (nth logs 125)
         (side-caption  "In fact, since the stacks are not shown, we can't even tell which order was taken.  "
           "In either case, from here we do whichever pop operation we didn't just do. "))
       (loginset
         (nth logs 126)
         (side-caption "Now we have " (fixed "(nil S)") " and " (fixed "(nil T)") ", "
           "and both stacks will be empty, so we are done!"))))))

(difflog/write-difflog
  "."
  "log.html"
  (first example1)
  (second example1))
