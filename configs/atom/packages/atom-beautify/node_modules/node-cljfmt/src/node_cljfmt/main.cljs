(ns node-cljfmt.main
  (:require
   [cljs.nodejs :as nodejs]
   [cljs.reader :as reader]
   [cljfmt.core :as cljfmt]
   [clojure.string :as string]))

(nodejs/enable-util-print!)

(def fs (nodejs/require "fs"))

(def known-option? #{:edn})

(defn parse-arg [parsed arg]
  (if (.startsWith arg "--")
    (let [[opt-str v] (string/split (subs arg 2) #"=" 2)
          opt         (keyword opt-str)]
      (assert (known-option? opt) (str "Unknown option " opt))
      (assoc-in parsed [:opts opt] v))
    (assoc parsed :filename arg)))

(defn parse-args [argv]
  (loop [[arg & args] (drop 2 argv)
         parsed {}]
    (if arg
      (recur args (parse-arg parsed arg))
      parsed)))

(defn edn-opts [edn-filename]
  (if edn-filename
    (reader/read-string ((aget fs "readFileSync") edn-filename "utf8"))
    {}))

(defn stdin-read [opts]
  (let [stdin (aget js/process "stdin")
        content (atom "")]
    (.call (aget stdin "setEncoding") stdin "utf8")
    (.call (aget stdin "on") stdin "readable" (fn [] (if-let [s (.call (aget stdin "read") stdin)] (swap! content str s))))
    (.call (aget stdin "on") stdin "end" (fn []
                                           (let [formatted (cljfmt/reformat-string @content opts)]
                                             (.log js/console formatted))))))

(defn -main []
  (let [parsed (parse-args (aget js/process "argv"))
        opts (edn-opts (get-in parsed [:opts :edn]))]
    (if-let [filename (:filename parsed)]
      (let [file ((aget fs "readFileSync") filename "utf8")
            formatted (cljfmt/reformat-string file opts)]
        ((aget fs "writeFileSync") filename formatted "utf8"))
      (stdin-read opts))))

(set! *main-cli-fn* -main)
