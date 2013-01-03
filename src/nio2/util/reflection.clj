(ns nio2.util.reflection
  (:import [clojure.lang IFn Var]
           [java.lang.reflect Method]))

;;https://gist.github.com/654851
(defn- declared-methods
  [^Class class]
  (filter #(= class (.getDeclaringClass ^Method %))
          (.getMethods class)))

(defn- reflection-accepts-arity?
  [func arity]
  (let [cls (class func)
        methods (declared-methods cls)]
    (or (when (instance? clojure.lang.RestFn func)
          (<= (.getRequiredArity ^clojure.lang.RestFn func) arity))
        (boolean (some (fn [^Method m]
                         (and (= "invoke" (.getName m))
                              (= (count (.getParameterTypes m)) arity)))
                       methods)))))

(defn- metadata-accepts-arity?
  [func arity]
  (reduce
    #(or
       %1
       (= arity (count %2))
       (and (= '& (last (butlast %2))) (>= arity (- (count %2) 2))))
   false
   (:arglists (meta func))))

(defmulti
  #^{:doc "Internal helper for accepts-arity"
     :private true
     :argslists '([func arity])}
  int-accepts-arity?
  (fn [func arity] (type func)))

;; Torn on if this should throw or not...
#_(defmethod int-accepts-arity? :default [func arity]
    false)

(defmethod int-accepts-arity? IFn [^IFn func arity]
  (reflection-accepts-arity? func arity))

(defmethod int-accepts-arity? Var [^Var func arity]
  (if (nil? (:arglists (meta func)))
    (reflection-accepts-arity? (deref func) arity)
    (metadata-accepts-arity? func arity)))

(defn accepts-arity?
  [func arity]
  (int-accepts-arity? func arity))


