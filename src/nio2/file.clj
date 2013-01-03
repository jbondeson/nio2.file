;; # The More New I/O APIs for the Java Platform (NIO.2) ... Wrapper
;;
;; The More New I/O APIs for the Java Platform, more succinctly
;; referred to as NIO.2, brings a host of changes to the file system
;; interface.

;; ## Entrée
;;
;; The `nio2.file` namespace is the base namespace that provides
;; generic facilities for working with some of the new types
;; introduced in NIO2 such as `Path` and `FileSystem`.

(ns nio2.file
  (:require [clojure.java.io :as io])
  (:import [java.io File]
           [java.nio.file FileSystems Path]))

(defonce ^:dynamic
  ^{:doc "Initialized to the default file system for the machine.
    Use this to override the file system to target for any operations
    that require it."}
  *file-system*
  (FileSystems/getDefault))

;; Make the `java.io.file.Path` object behave more like the other
;; built-in I/O types by extending the `Coercions` and `IOFactory`
;; protocols.
(extend Path
  clojure.java.io/Coercions
  {:as-file (fn [^Path p] (.toFile p))
   :as-url (fn [^Path p] (.toURL (.toURI p)))}
  clojure.java.io/IOFactory
  (assoc io/default-streams-impl
    :make-input-stream (fn [^Path x opts] (io/make-input-stream (.toFile x) opts))
    :make-output-stream (fn [^Path x opts] (io/make-output-stream (.toFile x) opts))))

(defn path?
  "Determines whether an object is of type Path."
  [x]
  (isa? (type x) Path))

(defn ^Path path
  "Generates a java.nio.file.Path object corresponding to the
   target of the catenation of the parts of the path supplied."
  [fst & more]
  (.getPath *file-system* fst (into-array String more)))

(defn file-like?
  "Determines whether an object can be treated like a File and
   points to a valid file on the disk."
  [x]
  (when-let [^File f (io/as-file x)]
    (.isFile f)))