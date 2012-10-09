(ns nio2.file
  (:require [clojure.java.io :as io])
  (:import [java.nio.file FileSystems Path]))

(def ^:dynamic
  *file-system*
  (FileSystems/getDefault))

(extend Path
  io/Coercions
  {:as-file (fn [^Path p] (.toFile p))
   :as-url (fn [^Path p] (.toURL (.toURI p)))}
  io/IOFactory
  (assoc io/default-streams-impl
    :make-input-stream (fn [^Path x opts] (io/make-input-stream (.toFile x) opts))
    :make-output-stream (fn [^Path x opts] (io/make-output-stream (.toFile x) opts))))

(defn- path? [x]
  (isa? (type x) Path))

(defn ^Path path [fst & more]
  "Generates a java.nio.file.Path object corresponding to the
   target of the catenation of the parts of the path supplied."
  (.getPath *file-system* fst (into-array String more)))