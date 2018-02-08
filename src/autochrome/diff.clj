(ns autochrome.diff
  (:require [autochrome.tree :as tree]
            [autochrome.parse :as parse])
  (:import [clojure.lang Util]
           [java.util HashMap PriorityQueue IdentityHashMap]))

(defn compare-vectors-by-identity
  [a b]
  (let [na (count a)]
    (if-not (= na (count b))
      false
      (loop [i 0]
        (if (= i na)
          true
          (when (identical? (nth a i) (nth b i))
            (recur (inc i))))))))

(deftype DiffContext [prevsources prevtargets]
  ;; `prevsources` and `prevtargets` are basically two independent stacks
  Object
  (hashCode [this]
    (Util/hashCombine
     (System/identityHashCode (peek prevsources))
     (System/identityHashCode (peek prevtargets))))
  (equals ^boolean [this that]
    (boolean
     (and (compare-vectors-by-identity (.-prevsources this) (.-prevsources that))
          (compare-vectors-by-identity (.-prevtargets this) (.-prevtargets that))))))

(deftype DiffState [cost sremain tremain source target context added deleted]
  ;; `source` and `target` are seqs, and we are diffing their heads
  ;; `cost` is the sum of the size of all the added or deleted nodes in this diff
  ;; `sremain` and `tremain` are the remaining sizes of the source/target form respectively
  ;; `context` is how we know 'where we are' in the source & target structures
  ;;   without context, we can't tell when we're finished, since all states
  ;;   where source=target=nil would be indistinguishable.
  ;; `added` and `deleted` are the cumulative adds and deletes from previous steps
  Object
  (hashCode [this]
    (unchecked-add-int
     (.hashCode context)
     (unchecked-add-int (System/identityHashCode source)
                        (System/identityHashCode target))))
  (equals [this that]
    (when-not (.-context this)
      (throw (IllegalStateException. "no this context")))
    (when-not (.-context that)
      (throw (IllegalStateException. "no that context")))
    (and (identical? (.-source this) (.-source that))
         (identical? (.-target this) (.-target that))
         (.equals (.-context this) (.-context that))))
  Comparable
  (compareTo [this that]
    (- (.-cost this) (.-cost that))))

;; for difflog
(def explored-states (atom []))
(def state-info (atom {}))

(defn dforms
  [source target]
  (let [size-map (doto (IdentityHashMap.) (.put nil 0) (tree/put-sizes source) (tree/put-sizes target))
        hashes (doto (IdentityHashMap.) (tree/put-hashes source) (tree/put-hashes target))
        start-state (DiffState. 0 (.get size-map source) (.get size-map target)
                      (:contents source) (:contents target)
                      (DiffContext. [] []) [] [])
        real-cost (doto (HashMap.) (.put start-state 0))
        pq (doto (PriorityQueue.) (.offer start-state))
        explore (fn [ncost predstate sremain tremain nsource ntarget nctx added deleted]
                  (let [ds (DiffState. (+ ncost (max sremain tremain)) sremain tremain nsource ntarget nctx added deleted)
                        prev-cost (.get real-cost ds)]
                    (swap! explored-states conj ds)
                    (swap! state-info update (System/identityHashCode ds) assoc :pred (System/identityHashCode predstate))
                    (when (or (nil? prev-cost) (< ncost prev-cost))
                      (swap! state-info update (System/identityHashCode ds) update :attrib conj
                        (if (nil? prev-cost) :best :better))
                      (.put real-cost ds ncost)
                      (.offer pq ds))))]
    (reset! explored-states [start-state])
    (reset! state-info {})
    (loop []
      (when-let [c (.poll pq)]
        (let [[shead & smore :as sforms] (.-source c)
              [thead & tmore :as tforms] (.-target c)
              cost (.get real-cost c)
              sremain (.-sremain c)
              tremain (.-tremain c)
              context (.-context c)
              prevsources (.-prevsources context)
              prevtargets (.-prevtargets context)]
          (swap! state-info update (System/identityHashCode c) update :attrib conj :popped)
          (if (and (nil? shead) (nil? thead) (empty? prevsources) (empty? prevtargets))
            c
            (let [ssize (.get size-map shead)
                  tsize (.get size-map thead)]
              ;; if we can match subtrees, don't bother doing anything else
              (if (and shead thead (= (.get hashes shead) (.get hashes thead)) (= shead thead))
                (explore cost c (- sremain ssize) (- tremain tsize) smore tmore context (.-added c) (.-deleted c))
                (do
                  (if shead
                    ;; deletion
                    (explore (+ cost ssize) c (- sremain ssize) tremain smore tforms context (.-added c) (conj (.-deleted c) shead))
                    ;; if we are at the end of the source seq, pop back out if we can
                    (when (not= 0 (count prevsources))
                      (explore cost c sremain tremain (peek prevsources) tforms
                        (DiffContext. (pop prevsources) prevtargets) (.-added c) (.-deleted c))))

                  (if thead
                    ;; addition
                    (explore (+ cost tsize) c sremain (- tremain tsize) sforms tmore context (conj (.-added c) thead) (.-deleted c))
                    ;; pop back out
                    (when (not= 0 (count prevtargets))
                      (explore cost c sremain tremain sforms (peek prevtargets)
                        (DiffContext. prevsources (pop prevtargets)) (.-added c) (.-deleted c))))
                  
                  ;; going into matching collections is a zero-cost operation
                  (when (and (tree/branch? shead) (tree/branch? thead) (= (:delim shead) (:delim thead)))
                    (explore cost c sremain tremain (tree/->children shead) (tree/->children thead)
                      (DiffContext. (conj prevsources smore) (conj prevtargets tmore)) (.-added c) (.-deleted c))) 
                  
                  ;; going into source node corresponds to stripping a pair of parens
                  (when (tree/branch? shead)
                    (explore (+ 2 cost) c sremain tremain (tree/->children shead) tforms
                      (DiffContext. (conj prevsources smore) prevtargets) (.-added c) (.-deleted c)))

                  ;; go into target node is wrapping with a new set of parens  
                  (when (and (tree/branch? thead))
                    (explore (+ 2 cost) c sremain tremain sforms (tree/->children thead)
                      (DiffContext. prevsources (conj prevtargets tmore)) (.-added c) (.-deleted c)))))
              (recur))))))))

(defn diffstate->annotations
  [dst]
  (let [ann (IdentityHashMap.)]
    (doseq [a (.-added dst)]
      (.put ann a :added))
    (doseq [d (.-deleted dst)]
      (.put ann d :deleted))
    ann))

(defn diff-forms
  [source target]
  (diffstate->annotations (dforms source target)))
