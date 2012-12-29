;; ## Efficient File and Directory Watching
;;
;; The More New I/O APIs for the Java Platform, more succinctly
;; referred to as NIO.2, (finally) bring abstracted file-system
;; watching capabilities to the JVM. The `nio2.file.watcher` namespace
;; provides a clojure wrapper for this functionality. The API was
;; inspired by the 'watchtower'[1] library written by Chris Granger.
;;
;; [1] https://github.com/ibdknox/watchtower
(ns nio2.file.watcher
  (:use [nio2.file]
        [nio2.util :only [accepts-arity?]])
  (:import [java.io File]
           [java.nio.file FileSystems Path StandardWatchEventKinds WatchService
            WatchKey]))

;; ## Convience wrappers for `java.nio.file` types

(def watch-events-map
  "Mapping of clojure symbols to watch event kinds."
  {:create StandardWatchEventKinds/ENTRY_CREATE
   :delete StandardWatchEventKinds/ENTRY_DELETE
   :modify StandardWatchEventKinds/ENTRY_MODIFY
   :overflow StandardWatchEventKinds/OVERFLOW})

(defn ^WatchService create-watch-service
  "Create a new WatchService on the active file system

   To change the target file system use `set!` or `binding` on
   the *file-system* var."
  []
  (.newWatchService *file-system*))

(defn close-watch-service
  "Close a WatchService object.

   Note that this will cause any calls to the service object to
   produce errors, and any calls on any derived WatchKey objects
   will similarly produce errors."
  [service]
  (.close service))

(defn ^WatchKey register-path
  "Registers a Path with a WatchService for the given events.

   The events should be a collection of keywords identifying the
   change type; `:create`, `:modify`, or `:delete`."
  [^Path p ^WatchService service events]
  (let [e (map #(% watch-events-map) events)]
    (.register p service (into-array e))))

;; ## Watch map creation

(def ^:dynamic *base-filter*
  "The first filter to apply to all paths prior to testing any
   user supplied filters."
  file-like?)

(defn build-changed-fn
  "Build a function to execute all the registered on-change
   handlers."
  [callbacks]
  ;; TODO: Test for arity
  ;; TODO: Change into a macro and flatten
  (fn [paths]
    (doseq [callback callbacks]
      (doseq [path paths]
        (callback path)))))

(defn build-filter-fn
  "Build a function that aggregates all the registered filters in
   addition to the filter stored in the *base-filter*"
  [filters]
  (let [filters (cons *base-filter* filters)
        filters (filter (complement nil?) filters)]
    (fn [f]
      (every? #(% f) filters))))

(defn get-updated-fn
  "Build a function to retrieve the enqueued events and their
   associaited paths."
  [key-map filters]
  (let [final-filter (build-filter-fn filters)
        valid? #(not= (.kind %) StandardWatchEventKinds/OVERFLOW)]
    (fn [^WatchKey watch-key]
      (let [^Path target (key-map watch-key)
            events (filter valid? (.pollEvents watch-key))
            resolve-context (fn ^Path [e] (->> e .context (.resolve target)))]
        (map resolve-context events)))))

(defn watcher*
  "Create a watch-map for the specified paths and events"
  [paths events]
  (let [to-set #(if (coll? %) (set %) #{%})
        paths (to-set paths)
        events (to-set events)
        service (create-watch-service)
        key-map (zipmap (map #(register-path % service events) paths) paths)]
    {:service service
     :key-map key-map
     :filters []
     :on-change nil}))

(defn compile-watcher
  "Compile the watch-map and generate the necessary functions."
  [{:keys [filters key-map on-change service] :as watch-map}]
  (assoc watch-map
    :get-updated (get-updated-fn key-map filters)
    :changed (build-changed-fn on-change)))

(defn watch
  "Start an infinite watch loop for the given watch-map.

   WARNING: This will not return until the watch service has been
   closed, and then it will throw an exception of type
   `ClosedWatchServiceException`. Always initiate on a separate
   thread of execution or through the `watcher` macro."
  [watch-map]
  (let [{:keys [get-updated changed service]} (compile-watcher watch-map)]
    (while true
      (let [watch-key (.take service)
            changes (get-updated watch-key)]
        (changed changes)
        (.reset watch-key)))))

;; ## Main API

(defmacro watcher
  "Create a watch-map watching the provided paths for a specific
   set of events.

   The paths should be a collection of strings representing directories
   to watch for changes from.

   The events should be a collection of keywords identifying the
   change type; `:create`, `:modify`, or `:delete`.

   The body should consist of calls to `on-change` and `path-filter`
   to setup the appropriate handlers and filters."
  [paths events & body]
  `(let [w# (-> (into [] (map path ~paths))
                (watcher* ~events)
                ~@body)
         f# (future (watch w#))]
     (assoc w# :future f#)))

(defn cancel-watcher
  "Cancel the watcher, and close the underlying WatchService"
  [{:keys [service future]}]
  (close-watch-service service)
  ;; Cancelling the future is probably not necessary as closing the
  ;; watch service will result in an exception being thrown, but it
  ;; certainly can't hurt.
  (future-cancel future))

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

