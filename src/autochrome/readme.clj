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
   [:.textcontainer {:width "57%"
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
  (let [aroot (parse/parse-one atext)
        broot (parse/parse-one btext)]
    (difflog/diff2 (diff/diff-forms aroot [broot]) aroot broot)))

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

(defn gen-readme
  []
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
        (comment "        # write a diff for a github pull request")

        "\n$ lein run --token user:123abc "(dom/i {} "owner") " " (dom/i {} "repo") " " (dom/i {} "num")
        (comment " # use supplied auth token for github api")

        "\n$ lein run --git-dir " (dom/i {} "/your/repo/ ") (dom/i {} "old-tree") " " (dom/i {} "new-tree")
        (comment "  # like git diff, using specified repo")

        "\n$ lein run --open ..."
        (comment "                         # try to open the diff in a browser"))
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
          "I ended up ditching A* for plain old Dijkstra's algorithm however - "
          (dom/a {:href "#alignment"} "more on that later") ".  "
          "Either way, in order to frame tree diffing as a pathfinding problem, you need to extend the concepts "
          "of location, cost, and adjacency to tree diffs.  Location is clearly needed to know where you are, "
          "but in addition locations need to be comparable, so you know not to bother when you already have a better "
          "path to the same place.  "
          "Cost is what makes some paths preferred over others.  For pathfinding on a road network, this would be "
          "the total distance traveled along the roads used. "
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
         "So let's look at a detailed log of the states we popped from our priority queue while generating the example diff at the top of this page.  "
         "The states are numbered in the order in which they were processed.  We will look at the goal state and each of its predecessors,  "
         "starting from the initial state.")))
    (let [a (parse/parse-one (first example1))
          b (parse/parse-one (second example1))
          logs (vec (difflog/diff-log a [b]))]
      (dom/div {:className "examplesection"}
               (loginset (first logs)
                         (side-caption "The source cursor is blue, and the target cursor is purple.  "
                                       "As you can see, we start with each cursor over its entire subtree.  "))
               (loginset
                (nth logs 1)
                (side-caption "After we enter the main loop and pop the start state, we can start exploring.  "
                              "In this state we have matched the parentheses and descended into the defn body. "
                              "Going into lists has cost 1, so that deleting an entire list "
                              "is cheaper than deleting each of its elements."))
               (loginset
                (nth logs 2)
                (side-caption "We matched " (fixed "defn") " with " (fixed "defn") " and advanced both cursors. "
                              "Now we can now match " (fixed "example") " with " (fixed "example") "."))
               (loginset
                (nth logs 3)
                (side-caption "Since matching is done with subtree hashes, we can match " (fixed "[x]")
                              " without going into the vector at all."))
               (loginset
                (nth logs 4)
                (side-caption
                 "Now we have our first mismatch.  We have a few options here:"
                 (dom/ol {}
                         (dom/li {} "Delete source (blue) subtree")
                         (dom/li {} "Add target (purple) subtree")
                         (dom/li {} "Go into both subtrees")
                         (dom/li {} "Go into blue subtree only")
                         (dom/li {} "Go into purple subtree only"))))
               (loginset
                (nth logs 8)
                (side-caption "We explore all of those options, but eventually we choose the last.  "
                              "Since we moved the target cursor into a list while the source cursor stayed put, "
                              "it follows that if we finish diffing, the parens which create that extra list must have been added, "
                              "so we can go ahead and paint them green, and add 2 to the cost."))
               (loginset
                (nth logs 11)
                (side-caption "Add the " (fixed "->") ".  It has size 2, but the new cost is 6.  This is because "
                              "each addition/deletion costs 1 extra point, so that "
                              "minimal diffs are cheaper than equivalent diffs with more changes. "))
               (loginset
                (nth logs 60)
                (side-caption " Delete " (fixed "(println \"hello\")") ".  Note that this is state #60 while the previous "
                              "state was #11 - we explored a whole bunch of dead-end states in between.  "
                              "This is because the deletion has a relatively high cost, so Dijkstra prefers to do "
                              "low- or no-cost movement before eventually getting around to this state."))
               (loginset
                (nth logs 63)
                (side-caption "Match the identical maps and advance each cursor.  "
                              "Since the map was the last element in the source defn body, the "
                              "source cursor has reached the end of its list, so there is nothing to highlight "
                              "in blue and it says " (fixed "(nil S)") " in the header."))
               (loginset
                (nth logs 65)
                (side-caption "It may look like nothing happened, but we popped out of the left subtree only here.  "
                              "This is an example of how movement operations get processed before any additions/deletions.  "
                              "It's completely free to explore here, so we might as well!"))
               (loginset
                 (nth logs 197)
                 (side-caption "Add " (fixed "(assoc :twice (+ x x))") ".  Another costly change means another big gap in state number.  "
                               "That was the last element in the "
                               "target sequence, so now we have " (fixed "(nil S)") " and " (fixed "(nil T)") ".  "))
               (loginset
                (nth logs 200)
                (side-caption  "Pop out of the " (fixed "(-> ...)") "."))
               (loginset
                (nth logs 203)
                (side-caption "Pop out of the target defn body.  "
                              "Now that we have popped all the way out of both forms, "
                              "both stacks are empty and there are no more forms to diff,  so we are done!"))))
    (dom/div
      {:className "textcontainer"
       :style {:margin "auto"}
       :id "alignment"}
      (section
        "Alignment"
        (p "I had originally implemented the diff algorithm as A*, which was a lot better at finding diffs with fewer explored states.  "
           "What made me decide to switch to plain Dijkstra's algorithm was the problem of alignment.  When multiple forms in a file "
           "are changed, inserted, renamed or deleted, how do you figure out which pairs to diff?"
           "A* works great when you know both the source and the target forms, but this proved difficult in practice.  ")
        (p "My first idea was to simply diff the entire source file with the entire target file, basically treating each file "
           "as if it had [] surrounding the entire thing.  This led to a lot of weird diffs; for example when you deleted "
           "something and inserted something else in its place, the diff would show how to transform the deleted thing "
           "into the new thing, which was confusing.  "
           "Top-level forms are the basic unit of clojure code, so diffs which span them are unnatural and hard to read.  "
           "When the change-of-nesting support was implemented, things really got out of hand.")
        (p "Something had to be done.  My next idea was to basically hack it by trying to match forms by their top-level text, "
           "for example 'defn somefn' or 'defmethod foo :dval'.  This has a lot of obvious problems, including docstrings, but "
           "especially renames.  It worked better than I expected but the problem was still not solved.")
        (p "The solution I came up with is to diff each target form in the old file against " (dom/i {} "all") " forms in the new file.  "
           "This is done by adding N start states to the priority queue, instead of only one, where N is the number of candidate target forms. "
           "Since A* really only makes sense in the context of single-source shortest paths, I decided to just switch to Dijkstra's algorithm,  "
           "which can deal just fine with multiple origins.  Since the diffs are processed in order of increasing cost, we know that "
           "the first complete diff we see will be the lowest-cost-possible diff of the source form with any of the target forms.  "
           "So we trade away single-target diff performance, but in return we get the guaranteed optimal solution to the alignment problem. ")
        (p "Doing diffs this way is technically quadratic, since in the worst case it requires every source form to be diffed against every "
           "target form, but there are a couple tricks that can be used to make it more palatable.  "
           "Most of the time, the majority of the forms in a file will be unchanged, so we can just hash everything first and match those "
           "right away.  That means the runtime is only quadratic with respect to the number of changed forms, which is better.  "
           "Second, each target form can only be matched to one source form, so we if we have to diff the first source against N targets, "
           "we only need to diff the second against N-1, and so on.  Still quadratic but oh well, parsing is usually slower anyway.  "
           "Finally, in each list of candidate targets we always include nil, representing the cost of deleting the entire source form.  "
           "This means no states more expensive than that are considered, which kind of controls the number of states we need to explore.")
        (p "There are a couple of slow cases, but for the most part I think the gains are worth the switch to Dijkstra.  "
           "Probably the slowest type of change to diff is splitting a very large form into two or more smaller forms, since we will spend "
           "a huge amount of time trying to figure out which smaller form is most similar to the original large form.  For example, "
           "If you split a 100-line function into two pieces and also make a bunch of changes, it might take like 30 seconds to diff.  "
           "That's not great, but you'll probably spend more than 30 seconds looking at a diff like that anyway."))))))

(do
  (gen-readme)
  ;(difflog/write-difflog "difflog" (first example1) [(second example1)])
  )
