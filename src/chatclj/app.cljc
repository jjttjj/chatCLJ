(ns chatclj.app
  (:require [clojure.string :as str]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [taoensso.timbre :as log]
            [nextjournal.markdown :as md]
            [nextjournal.markdown.transform :as md.transform]
            [clojure.walk :as walk]
            #?@(:clj [[chatclj.system :as sys]
                      [chatclj.openai :as openai]
                      [datomic.api :as d]
                      [clojure.java.io :as io]])
            #?@(:cljs [["markdown-it" :as MarkdownIt]]))
  (:import [hyperfiddle.electric Pending]))

(def models ["gpt-3.5-turbo" "gpt-4"])

(e/def db)
(e/def conn)

#?(:clj (defonce !msg-id->frags (atom {})))
(e/def msg-id->frags (e/server (e/watch !msg-id->frags)))

(defn msg-seq
  "Given a head (most recent) message map with recursive :msg/parent keys,
  return all messages in a flat sequence in the order they occur."
  [head]
  (->> head
       (iterate :msg/parent)
       (take-while some?)
       #_(map #(dissoc % :msg/parent))
       reverse))

#?(:clj
   (defn chat-messages
     "Return all messages as maps for the specified chat in chronological order."
     [db chat-id]
     ;; d/entity messes with reactivity, see
     ;; https://ask.datomic.com/index.php/859/equality-on-d-entity-ignores-db?show=859#q859
     #_(->> (d/entity db chat-id)
            :chat/head
            msg-seq
            (map d/touch))
     (->> (d/q
            '[:find (pull ?head [* {:msg/parent ...}]) .
              :in $ ?chat-id
              :where
              [?chat-id :chat/head ?head]]
            db
            chat-id)
          msg-seq)))

#?(:clj
   (defn current-chat-id [db]
     (some :v (d/datoms db :eavt [:user/id 0] :user/current-chat))))

(comment
  (let [db  @sys/latest-db]
    (->> (chat-messages db (current-chat-id db))
         (map :msg/content))))

#?(:clj
   (defn new-chat [conn opt]
     (let [chat-id (random-uuid)]
       @(d/transact conn
          [(merge {:db/id           -1
                   :chat/label      (str "chat " (subs (str (random-uuid)) 0 3))
                   :chat/model      "gpt-3.5-turbo"
                   :chat/streaming? true
                   :chat/head -2}
             opt)
           {:db/id -2
            :msg/role    :system
            :msg/content "You are a helpful assistant."}
           {:user/id           0
            :user/current-chat -1}]))))

(e/defn Sidebar [current-chat]
  (dom/div
    (dom/props {:class "sidebar"})
    (let [!model (atom (first models))
          model  (e/watch !model)]
      (dom/div
        (dom/props {:class "new-chat-container"})
        (dom/select
          (dom/props {:class "dropdown"})
          (dom/on "change"
            (e/fn [e] (reset! !model (.-value (.-target e)))))
          (e/for [opt models]
            (dom/option (dom/props {:value    opt
                                    :selected (when (= opt model) "selected")})
              (dom/text opt))))
        (dom/button
          (dom/props {:class "new-chat-button"})
          (dom/on "click"
            (e/fn [e]
              (e/server
                (e/discard
                  (new-chat conn {:chat/model model})))))
          (dom/text "New Chat"))))
    (dom/ul
      (e/server
        (e/for-by :db/id
          ;; TODO: sort by some ts
          [chat (d/q '[:find [(pull ?chat [*]) ...]
                       :where [?chat :chat/label]]
                  db)]
          (e/client
            (dom/li
              (dom/on "click"
                (e/fn [e]
                  (e/server
                    @(d/transact conn
                       [{:user/id           0
                         :user/current-chat chat}])
                    nil)))
              (dom/props {:class ["sidebar-chat"
                                  (if (= (:db/id chat) current-chat) "current-chat")]})
              (dom/text (:chat/label chat))
              (dom/span (dom/props {:class "material-icons"
                                    :style {:cursor "pointer"}})
                (dom/on "click"
                  (e/fn [e]
                    (.preventDefault e)
                    (e/server
                      (e/discard
                        (log/info "deleting chat" (:db/id chat))
                        @(d/transact conn [[:db/retractEntity (:db/id chat)]])))))
                (dom/text "delete")))))))))

(defn- truncate-string [s limit]
  (if (> (count s) limit)
    (str (subs s 0 limit) "...")
    s))

#?(:clj
   ;; TODO: split this up into tx functions, ie (fn [db ...]) => txs
   ;; Note: cannot be used in reactive code due to d/entity usage
   (defn new-msg [conn chat-id content]
     (let [db        (d/db conn)
           chat      (d/entity db chat-id)
           msgs      (msg-seq (:chat/head chat))
           #_        (chat-messages db chat-id)

           parent-id (:db/id (last msgs))
           _         (assert parent-id)
           our-msg   {:db/id       -1
                      :msg/content content
                      :msg/role    :user
                      :msg/parent  parent-id}

           ;; placeholder resp
           resp-placeholder
           {:db/id      -2
            :msg/role   :assistant
            :msg/parent (:db/id our-msg)}

           {:keys [tempids] :as tx}
           @(d/transact conn
              [our-msg resp-placeholder
               [:db/add chat-id :chat/head -2]])

           with-ours (conj (vec msgs) our-msg)
           req
           {:model    (:chat/model chat)
            :messages (keep (fn [{:msg/keys [role content]}]
                              ;; This lets us avoid sending empty
                              ;; messages which can occur currently if
                              ;; a response timed out and thus the
                              ;; placeholder content never arrives
                              (when content
                                {:role    role
                                 :content content}))
                        with-ours)
            :stream   (:chat/streaming? chat)}

           ;; actual id after transacting the placeholder
           resp-msg-id (tempids (:db/id resp-placeholder))

           _ (log/infof "making openapi request for prompt: \"%s\""
               (truncate-string content 50))

           resp-msg
           (if (:chat/streaming? chat)
             (transduce
               identity
               (fn
                 ([] {:content ""})
                 ([pending-msg frag]
                  (swap! !msg-id->frags update resp-msg-id (fnil conj []) frag)
                  (let [delta (some-> frag :choices first :delta)]
                    (if-let [role (:role delta)]
                      (assoc pending-msg :role role)
                      (if-let [content (:content delta)]
                        (update pending-msg :content str content)
                        pending-msg))))
                 ([msg]
                   (swap! !msg-id->frags dissoc resp-msg-id)
                   msg))
               (openai/streaming-request req))
             ;; else, non-streaming request
             (-> (openai/request req) :choices first :message))

           _ (log/infof "response received: \"%s\"" (truncate-string (:content resp-msg) 50))

           resp-tx (-> resp-msg
                       ;; resp -> entity
                       (update :role keyword)
                       (update-keys (fn [k] (keyword "msg" (name k))))
                       ;; to update our existing pending response
                       (assoc :db/id resp-msg-id))]
       @(d/transact conn [resp-tx])
       resp-tx)))

(comment
  ;; add a new message to current chat from the repl
  (new-msg sys/conn
    (current-chat-id @sys/latest-db)
    "what's 1+1")
  )


;;; Note: hiccup ids not currently supported in tags (ie :div#my-id)
(e/def H2e)
(e/defn ElectricMarkdown [text]
  (let [data   (md/parse text)
        hiccup (md.transform/->hiccup data)]
    (binding [H2e (e/fn [hiccup-data]
                    (let [[tag & tagclasses] (str/split (name (first hiccup-data)) #"\.")
                          [children attrs] (if (map? (second hiccup-data))
                                             [(second hiccup-data) (drop 2 hiccup-data)]
                                             [(drop 1 hiccup-data)])
                          attrs (cond-> attrs
                                  (seq tagclasses) (update :class concat tagclasses))]
                      (dom/with (dom/new-node dom/node tag)
                        (when (seq attrs)
                          (dom/props attrs))
                        (e/on-unmount #(set! (.. dom/node -style -display) "none")) ; hack
                        (e/for [child children]
                          (if (string? child)
                            (dom/text child)
                            (H2e. child))))))]
      (H2e. hiccup))))


;;; Alternative option: just use markdown-it and set the innerHTML
#?(:cljs
   (let [md (MarkdownIt. "default" #_"zero" #_"commonmark")]
     (defn parse-markdown [s]
       (.render md s))))

(e/defn ElectricMarkdown2 [text]
  (dom/span (dom/props {:class "markdown"})
    (set! (.-innerHTML dom/node)
      (parse-markdown text))))


(e/defn Content [chat-id]
  (e/client
    (let [chat (e/server (d/pull db '[*] chat-id))
          msgs (e/server (chat-messages db chat-id))]
      (dom/div
        (dom/props {:class "content"})
        (dom/div
          (dom/props {:class "chat-header"})
          (dom/h1 (dom/props {:contenteditable true})
            (let [save-label
                  (e/fn [e]
                    (if-let [label (not-empty (.. e -target -textContent))]
                      (do (log/info "new :chat/label" label)
                          (e/server
                            (e/discard
                              (d/transact conn
                                [[:db/add (:db/id chat) :chat/label label]]))))
                      (log/error "empty label")))]
              (dom/on "blur" save-label)
              (dom/on "keydown"
                (e/fn [e]
                  (when (= "Enter" (.-key e))
                    (.preventDefault e)
                    (new save-label e)
                    (.. e -target blur)))))
            (dom/text (:chat/label chat)))
          (dom/span (dom/props {:class "model-info"})
            (dom/text (:chat/model chat)))
          (dom/div
            (dom/props {:class "header-options"})

            (dom/div (dom/props {:class "checkbox-container"})
              (dom/label (dom/props {:for "streaming-checkbox", :class "checkbox-label"}) (dom/text "Streaming?"))
              (dom/input
                (dom/props {:type    "checkbox", :id "streaming-checkbox", :class "checkbox"
                            :checked (when (:chat/streaming? chat) "checked")})
                (dom/on "change"
                  (e/fn [e]
                    (let [streaming? (.. e -target -checked)]
                      (log/info "changing :chat/streaming? to" streaming?)
                      (e/server
                        (e/discard
                          @(d/transact conn
                             [[:db/add (:db/id chat) :chat/streaming? streaming?]]))))))))))

        (dom/ul (dom/props {:class "message-area"})
          (e/server
            (e/for-by :db/id [{:keys [msg/content msg/role db/id] :as msg} msgs]
              (e/client
                (dom/li
                  (dom/props {:class "message"})
                  (when role (dom/text (name role) " > "))
                  (if content
                    #_(dom/text content) ;; raw text
                    (ElectricMarkdown. content)
                    (e/server
                      (e/for-by identity [frag (get msg-id->frags id)]
                        (let [delta (some-> frag :choices first :delta)]
                          (when-let [content (:content delta)]
                            (e/client (dom/span (dom/text content))))))))
                  (do
                    ;; Every time we see a new msg in for-by, we scroll to the bottom
                    (.scrollIntoView
                      (.getElementById js/document "msg-bottom")
                      #_dom/node))))))
          (dom/div
            (dom/props {:style {:display "none"}
                        :id    "msg-bottom"})))

        (dom/div
          (dom/textarea
            (dom/props {:class       "input-area"
                        :placeholder "Type a message"})
            (dom/on "keydown"
              (e/fn [e]
                (when (= "Enter" (.-key e))
                  (.preventDefault e)
                  (when-some [v (not-empty (.. e -target -value))]
                    (set! (.-value dom/node) "")

                    (e/server
                      (e/discard
                        (future
                          (try
                            (new-msg conn chat-id v)
                            (catch Throwable t
                              (log/error t "error adding new-msg"))))))))))))))))


(e/defn Main []
  (e/server
    (binding [db (e/watch sys/latest-db)
              conn sys/conn]
      (e/client
        (try
          (dom/div
            (dom/link
              (dom/props
                {:rel  "stylesheet"
                 :href "https://fonts.googleapis.com/icon?family=Material+Icons"}))
            (dom/props {:class "app"})
            (let [current-chat (e/server (current-chat-id db))]
              (Sidebar. current-chat)
              (when current-chat (Content. current-chat))))
          (catch Pending e
            (dom/style {:background-color "yellow"})))))))
