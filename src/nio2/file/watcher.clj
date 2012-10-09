(ns nio2.file.watcher
  (:use [nio2.file]
        [nio2.util :only [accepts-arity?]])
  (:import [java.io File]
           [java.nio.file FileSystems Path StandardWatchEventKinds WatchService
            WatchKey]))

;; ## Convience wrappers for `java.nio.file` types

(def ^:private events
  {:create StandardWatchEventKinds/ENTRY_CREATE
   :delete StandardWatchEventKinds/ENTRY_DELETE
   :modify StandardWatchEventKinds/ENTRY_MODIFY
   :overflow StandardWatchEventKinds/OVERFLOW})

(defn- ^WatchService
  watch-service []
  (.newWatchService *file-system*))

(defn- ^WatchKey
  register [^Path p ^WatchService service events]
  (.register p service (into-array events)))

;; ## Watch map creation

(defn watcher*
  [paths events]
  (let [to-coll (fn [c] (if (coll? c) c [c]))
        paths (to-coll paths)
        events (to-coll events)
        service (watch-service)
        key-map (zipmap (map #(register % service events) paths) paths)]
    {:service service
     :key-map key-map
     :filters []
     :on-change nil}))

(defn path-filter
  "Add a filter to a watcher.

   A filter should be a single-parameter function taking a
   `java.nio.file.Path` object and returns true if the path is
   to be included, and false otherwise."
  [w filt]
  (update-in w [:filters] conj filt))

(defn on-change
  "Add a change handler to a watcher

   A change handler should be a one or two parameter function
   taking a `java.nio.file.Path` object and (if it accepts two)
   a keyword identifying the change type; `:create`, `:modify`,
   or `:delete`."
  [w func]
  (update-in w [:on-change] conj func))

