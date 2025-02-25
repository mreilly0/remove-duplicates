(ns user
  (:require [clojure.java.io :as io]
            [rewrite-clj.zip :as z]))

(defn paths
  [m]
  (letfn [(helper [m path]
            (cond (set? m) (mapcat (fn [i] (helper i (conj path i))) m)
                  (map? m) (mapcat (fn [[k v]] (helper v (conj path k))) m)
                  (coll? m) (apply concat
                              (map-indexed (fn [k v] (helper v (conj path k)))
                                           m))
                  :else [path]))]
    (vec (helper m []))))

(defn super-paths
  [m]
  (let [paths (paths m)
        paths (reduce (fn outer [acc p]
                        (loop [acc acc
                               p p]
                          (if (empty? p) acc (recur (conj acc p) (pop p)))))
                #{}
                paths)]
    (try (into (sorted-set) paths) (catch Exception _not-comparable paths))))

(defn get+
  [m k]
  (cond (set? m) (get m k)
        (associative? m) (get m k)
        :else (nth m k)))


(defn get-in+
  "like get-in but when m is a list use nth instead of get for lookup"
  [m ks]
  (reduce (fn [m k] (get+ m k)) m ks))

(defn- assoc+
  [m k v]
  (cond (set? m) (throw (ex-info "todo" {:m m :v v}))
        (associative? m) (assoc m k v)
        :else (concat (take k m) (cons v (drop (inc k) m)))))

(defn assoc-in+
  "like assoc-in but also works when m is a list"
  [m [k & ks] v]
  (if ks (assoc+ m k (assoc-in+ (get+ m k) ks v)) (assoc+ m k v)))

(defn get-forms
  [file]
  (with-open [r (io/reader file)
              p (java.io.PushbackReader. r)]
    (loop [acc []]
      (let [form (try (read p) (catch Exception _done-reading))]
        (if form (recur (conj acc form)) acc)))))

(get-forms
  "b.clj")

(defn find-duplicate-map
  [file]
  (let [forms (get-forms file)
        paths (vec (super-paths forms))
        len (count paths)
        maps (loop [i 0
                    maps {}]
               (if (>= i len)
                 maps
                 (let [p (get paths i)
                       o (get-in+ forms p)]
                   (recur (inc i) (if (map? o) (update maps o conj p) maps)))))]
    (reduce (fn [acc [m paths]]
              (if (and (> (count paths) 1) (> (count (str m)) 50))
                (assoc acc m paths)
                acc))
      {}
      maps)))

(defn find-paths-of
  [forms v]
  (let [paths (vec (super-paths forms))]
    (filter (fn [path] (= v (get-in+ forms path))) paths)))

(defn fix-ns
  "Return the contents of clj-filename as a String, replacing all
  occurrences of the map m with m-name."
  [clj-filename m m-name]
  (let [root (z/of-file clj-filename)]
    (loop [loc root]
      (if (z/end? loc)
        (z/root-string loc)
        (recur (z/next (if (and (z/map? loc) (= m (z/sexpr loc)))
                         (z/replace loc m-name)
                         loc)))))))
(comment

  (spit
    "a.clj"
    (fix-ns
      "b.clj"
      {:foo "bar"}
      'paddy-23))


  (first
    (sort-by
      (fn [[k v]] (- (count v)))
      (find-duplicate-map
        "b.clj")))


  (loop [files                (line-seq (io/reader "a.txt"))
         biggest-dupe-count   0
         file-with-most-dupes nil]
    (if-let [file (first files)]
      (let [dupes              (find-duplicate-map file)
            current-dupe-count (count dupes)
            [biggest-dupe-count file-with-most-dupes]
            (if (> current-dupe-count biggest-dupe-count)
              [current-dupe-count file]
              [biggest-dupe-count file-with-most-dupes])]
        (recur (rest files) biggest-dupe-count file-with-most-dupes))
      file-with-most-dupes)))
