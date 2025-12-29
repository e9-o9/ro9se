#!/usr/bin/env clojure
;; opencog-atomspace.clj - Hypergraph in Clojure
(ns opencog.atomspace)

(def atom-types #{:atom :node :link :concept-node :predicate-node :inheritance-link :evaluation-link})

(defrecord TruthValue [strength confidence])
(defn make-tv [& {:keys [s c] :or {s 1.0 c 1.0}}] (->TruthValue s c))

(defrecord Node [type name tv id])
(defrecord Link [type outgoing tv id])

(defn make-atomspace []
  {:atoms (atom [])
   :node-index (atom {})
   :type-index (atom {})
   :incoming-index (atom {})
   :id-counter (atom 0)})

(defn add-node! [as type name]
  (if-let [existing (get-in @(:node-index as) [type name])]
    existing
    (let [id (swap! (:id-counter as) inc)
          node (->Node type name (make-tv) id)]
      (swap! (:atoms as) conj node)
      (swap! (:node-index as) assoc-in [type name] node)
      (swap! (:type-index as) update type (fnil conj []) node)
      node)))

(defn add-link! [as type outgoing]
  (let [id (swap! (:id-counter as) inc)
        link (->Link type outgoing (make-tv) id)]
    (swap! (:atoms as) conj link)
    (swap! (:type-index as) update type (fnil conj []) link)
    (doseq [atom outgoing]
      (swap! (:incoming-index as) update (:id atom) (fnil conj []) link))
    link))

(defn demonstrate-atomspace []
  (println "=================================================================")
  (println "OpenCog AtomSpace - Clojure Hypergraph Implementation")
  (println "=================================================================\n")
  
  (let [as (make-atomspace)
        human (add-node! as :concept-node "human")
        mortal (add-node! as :concept-node "mortal")
        socrates (add-node! as :concept-node "Socrates")]
    
    (add-link! as :inheritance-link [socrates human])
    (add-link! as :inheritance-link [human mortal])
    
    (println "AtomSpace size:" (count @(:atoms as)))
    (println "Concept nodes:" (count (get @(:type-index as) :concept-node)))
    (println "\nClojure AtomSpace: Immutable data, persistent structures, atoms for state")))

(demonstrate-atomspace)
