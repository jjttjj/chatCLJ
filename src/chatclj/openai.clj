(ns chatclj.openai
  (:require [babashka.http-client :as http]
            [babashka.json :as json]
            [clojure.core.async :as a]
            [clojure.java.io :as io]
            [net.cgrand.xforms.io :as xio])
  (:import [com.knuddels.jtokkit Encodings]
           [com.knuddels.jtokkit.api EncodingType]
           [com.knuddels.jtokkit.api ModelType]
           [java.io Reader BufferedReader]
           [clojure.lang IFn Fn IReduce]))

(def api-key
  (System/getenv "OPENAI_API_KEY"))

(defn request [req-data]
  (let [resp (http/post "https://api.openai.com/v1/chat/completions"
               {:headers {"Content-Type"  "application/json"
                          "Authorization" (str "Bearer " api-key)}
                :body    (json/write-str req-data)})]
    (-> (:body resp)
        json/read-str
        (with-meta resp))))

(defn streaming-request
  "Sets `:stream` to true on request body. Returns eduction of parsed responses."
  [req-data]
  (let [resp (http/post "https://api.openai.com/v1/chat/completions"
               {:headers {"Content-Type"  "application/json"
                          "Authorization" (str "Bearer " api-key)}
                :body    (json/write-str (assoc req-data :stream true))
                :as      :stream})]
    (eduction
      (comp
        (take-while #(not= % "data: [DONE]"))
        ;; SSE request are split with \n\n, so we get an extra blank for each line:
        (remove #(= % ""))
        (keep (fn [s] (second (re-find #"data:\s+(.+)" s))))
        (map json/read-str))
      (xio/lines-in (:body resp)))))



(comment
  (def example-request
    {:model    "gpt-3.5-turbo"
     :messages [{:role    "system"
                 :content "You are a helpful assistant."}
                {:role    "user"
                 :content "Tell me a joke about clojure"}]})

  (request example-request)

  (transduce
    (map (fn [x]
           (let [delta (some-> x :choices first :delta)]
             (if-let [role (:role delta)]
               (println (str role ":"))
               (when-let [content (:content delta)]
                 (print content)
                 (flush)
                 content)))))
    str
    (streaming-request example-request))
  )


;;; wip: token counting
(comment

  (def registry (Encodings/newDefaultEncodingRegistry))
  (def encoding (.getEncoding registry EncodingType/CL100K_BASE))


  ;; see https://github.com/openai/openai-cookbook/blob/12527df1f98a3aa280034ea788f82d58d9a71e08/examples/How_to_count_tokens_with_tiktoken.ipynb
  (defn count-tokens [model content]
    (assert (#{"gpt-4" "gpt-3.5-turbo"} model))
    (case model
      ("gpt-4" "gpt-3.5-turbo")
      (+ 3 1 (count (.encode encoding content)))))

  (count-tokens "gpt-4" "hello, world")
  )
