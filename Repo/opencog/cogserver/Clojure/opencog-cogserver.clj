#!/usr/bin/env clojure
;; opencog-cogserver.clj - Network Server in Clojure
(ns opencog.cogserver
  (:require [clojure.string :as str]))

(defrecord CommandResult [success message])
(defn make-success [msg] (->CommandResult true msg))
(defn make-failure [msg] (->CommandResult false msg))

(defn make-registry []
  (atom {}))

(defn register-command! [registry name handler desc]
  (swap! registry assoc name {:handler handler :desc desc}))

(defn execute-command [registry name args]
  (if-let [cmd (get @registry name)]
    (try
      ((:handler cmd) args)
      (catch Exception e
        (make-failure (str "Error: " (.getMessage e)))))
    (make-failure (str "Unknown command: " name))))

(defrecord Session [id created-at data authenticated])

(defn make-cogserver [name]
  {:name name
   :registry (make-registry)
   :sessions (atom {})})

(defn register-builtin-commands! [server]
  (let [reg (:registry server)]
    (register-command! reg "help" 
      (fn [_] (make-success (str "Commands: " (str/join ", " (keys @reg)))))
      "Show commands")
    (register-command! reg "version"
      (fn [_] (make-success "OpenCog Clojure v1.0.0"))
      "Show version")
    (register-command! reg "echo"
      (fn [args] (make-success (str/join " " args)))
      "Echo arguments")))

(defn demonstrate-cogserver []
  (println "==================================================================")
  (println "OpenCog CogServer - Clojure Implementation")
  (println "==================================================================\n")
  
  (let [server (make-cogserver "TestServer")]
    (register-builtin-commands! server)
    
    (println "Commands:" (keys @(:registry server)))
    (println "\nExecuting version:")
    (println (:message (execute-command (:registry server) "version" [])))
    (println "\nClojure CogServer: Atoms, persistent data, functional design")))

(demonstrate-cogserver)
