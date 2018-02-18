(ns autochrome.align
  (:require [autochrome.diff :as diff]
            [autochrome.tree :as tree]
            [autochrome.parse :as parse])
  (:import [java.util IdentityHashMap Map]))

(defn get-diffs
  [source-forms target-forms]
  (let [prep (diff/diff-prep source-forms target-forms)
        ^IdentityHashMap hashes (:hashes prep)
        ^IdentityHashMap sizes (:sizes prep)

        hash->source-form (zipmap (map #(.get hashes %) source-forms) source-forms)
        ^IdentityHashMap matched-forms (IdentityHashMap.)
        unmatched? #(not (.containsKey matched-forms %))]

    (doseq [tf target-forms]
      (when-let [sf (hash->source-form (.get hashes tf))]
        (.put matched-forms tf :matched)
        (.put matched-forms sf :matched)))

    (loop [diffs []
           [the-source & sources] (filterv unmatched? source-forms)
           targets (filterv unmatched? target-forms)]
      (cond
        (and (nil? the-source) (empty? targets))
        diffs

        (nil? the-source)
        (concat diffs
                (for [t targets]
                  [nil t (doto (IdentityHashMap.) (.put t :added))]))

        (empty? targets)
        (concat diffs
                (for [s sources]
                  [s nil (doto (IdentityHashMap.) (.put s :deleted))]))

        :else
        (let [goal (diff/dforms the-source (cons nil targets) hashes sizes)
              the-target (diff/get-target goal)]
          ;(println 'goal-cost (diff/get-cost goal) 'nstates (count @diff/explored-states) 'npopped @diff/npopped)
          ;(println 'source (parse/render the-source))
          ;(println 'target (parse/render the-target))
          (.put matched-forms the-target :matched)
          (recur
            (conj diffs [the-source the-target (diff/diffstate->annotations goal)])
            sources
            (filter unmatched? targets)))))))
