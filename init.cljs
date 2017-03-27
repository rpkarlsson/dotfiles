#!/usr/local/bin/planck
(ns init.core
  (:require [planck.shell :refer [sh]]
            [planck.io :refer [file-attributes]]
            [planck.core :refer [file-seq]]
            [clojure.string :as str]))

(def home (str "/Users/" (str/trim-newline (:out (sh "whoami"))) "/"))
(def current-dir (str/trim-newline (:out (sh "pwd"))))

(defn wanted? [f]
  (let [split-path (str/split (:path f) "/")
        blacklist ["init.clj" "init.lisp" ".git"]]
    (not
     (or
      (= (count split-path) 1)
      (some #{(second split-path)} blacklist)))))

(defn create-link [f]
  (let [filename (subs (:path f) 2)
        source (str current-dir "/" filename)
        target (str home "." filename)]
    (sh "ln" "-s" source target)))

(defn create-folders [fs]
  (doseq [f fs]
    (when (= :directory (:type (file-attributes f)))
      (sh "mkdir" (str home "." (subs (:path f) 2)))))
  fs)

(defn link-files [fs]
  (doseq [f fs]
    (when-not (= :directory (:type (file-attributes f)))
      (create-link f)))
  fs)

(-> (filter wanted? (file-seq "."))
    (create-folders)
    (link-files))
