#!/usr/bin/env clojure
;; opencog-cogutil.clj
;; OpenCog Cogutil - Clojure Utility Library
;; Demonstrates: Immutable data structures, persistent data, STM, protocols

(ns opencog.cogutil
  (:require [clojure.string :as str]))

;; ===== Logger System =====
;; Demonstrates: Protocols, records, multimethods

(def log-levels {:debug 0 :info 1 :warn 2 :error 3})

(defprotocol ILogger
  (log-message [this level msg])
  (set-level! [this level]))

(defrecord Logger [name level-atom]
  ILogger
  (log-message [this lvl msg]
    (when (>= (log-levels lvl) (log-levels @level-atom))
      (println (format "[%s] %s: %s" 
                      (.format (java.time.LocalTime/now) 
                              (java.time.format.DateTimeFormatter/ofPattern "HH:mm:ss"))
                      (str/upper-case (name lvl))
                      msg))))
  (set-level! [this lvl]
    (reset! level-atom lvl)))

(defn make-logger [name & {:keys [level] :or {level :info}}]
  (->Logger name (atom level)))

(defn debug [logger msg] (log-message logger :debug msg))
(defn info [logger msg] (log-message logger :info msg))
(defn warn [logger msg] (log-message logger :warn msg))
(defn error [logger msg] (log-message logger :error msg))

;; ===== Configuration Manager =====
;; Demonstrates: Atoms for mutable state, persistent maps

(defn make-config []
  (atom {}))

(defn config-set! [config k v]
  (swap! config assoc k v))

(defn config-get [config k & [default]]
  (get @config k default))

(defn config-has? [config k]
  (contains? @config k))

(defn config-dump [config]
  (doseq [[k v] (sort @config)]
    (println (str k " = " v))))

;; ===== Timer Utility =====
;; Demonstrates: Closures, time handling

(defn make-timer [name logger]
  (let [start-time (atom nil)]
    {:start (fn [] (reset! start-time (System/nanoTime)))
     :stop (fn []
             (when @start-time
               (let [elapsed (/ (- (System/nanoTime) @start-time) 1e9)]
                 (when logger
                   (info logger (format "%s completed in %.6f seconds" name elapsed)))
                 elapsed)))}))

;; ===== String Utilities =====
;; Demonstrates: Functional programming, immutable strings

(defn str-split [text delimiter]
  (->> (str/split text (re-pattern (java.util.regex.Pattern/quote delimiter)))
       (map str/trim)
       (remove str/blank?)))

(defn str-join [strings delimiter]
  (str/join delimiter strings))

(defn to-lower [text] (str/lower-case text))
(defn to-upper [text] (str/upper-case text))
(defn trim [text] (str/trim text))

;; ===== Demonstration =====

(defn demonstrate-cogutil []
  (println (apply str (repeat 70 \=)))
  (println "OpenCog Cogutil - Clojure Utility Library Demo")
  (println "Showcasing: Immutable data, STM, protocols, persistent structures")
  (println (apply str (repeat 70 \=)))
  (println)
  
  ;; Logger
  (println "1. Logger with Protocols")
  (println (apply str (repeat 50 \-)))
  (let [logger (make-logger "CogUtil")]
    (info logger "Cogutil library initialized")
    (debug logger "This won't show")
    (warn logger "Warning message")
    (set-level! logger :debug)
    (debug logger "Now visible"))
  (println)
  
  ;; Config
  (println "2. Configuration with Atoms")
  (println (apply str (repeat 50 \-)))
  (let [config (make-config)]
    (config-set! config "opencog.version" "1.0.0")
    (config-set! config "atomspace.enabled" "true")
    (println "Configuration:")
    (config-dump config))
  (println)
  
  ;; Timer
  (println "3. Timer with Closures")
  (println (apply str (repeat 50 \-)))
  (let [logger (make-logger "CogUtil")
        timer (make-timer "Processing" logger)]
    ((:start timer))
    (Thread/sleep 10)
    ((:stop timer)))
  (println)
  
  ;; Immutability
  (println "4. Immutable Data Structures")
  (println (apply str (repeat 50 \-)))
  (let [data {:name "OpenCog" :version "1.0"}
        data2 (assoc data :language "Clojure")]
    (println "Original:" data)
    (println "Modified:" data2)
    (println "Original unchanged:" data))
  (println)
  
  (println (apply str (repeat 70 \=)))
  (println "Clojure strengths:")
  (println "  ✓ Immutable persistent data structures")
  (println "  ✓ Software Transactional Memory (STM)")
  (println "  ✓ Protocols for polymorphism")
  (println "  ✓ Functional programming")
  (println "  ✓ JVM interoperability")
  (println (apply str (repeat 70 \=))))

(demonstrate-cogutil)
