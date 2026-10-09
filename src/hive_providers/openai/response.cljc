(ns hive-providers.openai.response
  "Chat-completions responses: whole bodies, server-sent events, and the
   fold of a stream back into one message.

   A MESSAGE is {:role :content :tool_calls :finish_reason :usage :model}
   plus any reasoning keys the provider sent (:reasoning, :reasoning_content,
   :reasoning_details; :reasoning is filled from :reasoning_content when
   only that is present). :tool_calls arguments are always JSON text.

   SPDX-License-Identifier: MIT"
  (:require [clojure.string :as str]
            [hive-providers.codec.json :as json]
            [hive-providers.result :as r]
            [hive-providers.text :as text]))

(defn- reasoning-of
  [source]
  (let [kept (select-keys source [:reasoning :reasoning_content :reasoning_details])
        kept (into {} (remove (fn [[_ v]] (nil? v))) kept)]
    (if (and (nil? (:reasoning kept)) (:reasoning_content kept))
      (assoc kept :reasoning (:reasoning_content kept))
      kept)))

(defn tool-call
  "Canonical tool call for the wire TC; :arguments as JSON text."
  [tc]
  (let [f    (:function tc)
        args (:arguments f)]
    (cond-> {:id       (:id tc)
             :type     (or (:type tc) "function")
             :function {:name      (:name f)
                        :arguments (cond (string? args) args
                                         (nil? args)    ""
                                         :else          (json/write-str args))}}
      (some? (:index tc)) (assoc :index (:index tc)))))

(defn- message-of
  [choice]
  (let [m (:message choice)]
    (cond-> (merge {:role (or (:role m) "assistant") :content (:content m)}
                   (reasoning-of m))
      (seq (:tool_calls m))   (assoc :tool_calls (mapv tool-call (:tool_calls m)))
      (:finish_reason choice) (assoc :finish_reason (:finish_reason choice)))))

(defmulti extras
  "Provider-specific extras of a parsed response BODY, dispatched on the
   entry's :response-extras keyword. Default: none."
  (fn [kind _body] kind))

(defmethod extras :default [_ _] nil)

(defmethod extras :perplexity [_ body]
  (not-empty (select-keys body [:citations :search_results :return_images])))

(defn body->message
  "Result: the first choice's MESSAGE for the parsed BODY. EXTRAS-KIND names
   an `extras` method. err :llm/empty-choices when there is no choice, or
   :llm/api-error when the body carries an error object."
  ([body] (body->message body nil))
  ([body extras-kind]
   (cond
     (:error body)
     (r/err :llm/api-error {:detail (:error body)})

     (empty? (:choices body))
     (r/err :llm/empty-choices {:body body})

     :else
     (let [msg (assoc (message-of (first (:choices body)))
                      :usage (:usage body)
                      :model (:model body))
           x   (when extras-kind (extras extras-kind body))]
       (r/ok (if x (assoc msg :extras x) msg))))))

(defn parse-body
  "Result: the MESSAGE in the response body TEXT."
  ([text] (parse-body text nil))
  ([text extras-kind]
   (r/bind (json/read-str text) #(body->message % extras-kind))))

;; =============================================================================
;; Server-sent events
;; =============================================================================

(defn sse-data
  "The data of one SSE LINE: nil for a non-data line, :done for [DONE],
   else the parsed JSON map (nil when it does not parse)."
  [line]
  (let [line (str/trim (str line))]
    (when (str/starts-with? line "data:")
      (let [data (str/trim (subs line 5))]
        (if (= data "[DONE]")
          :done
          (json/read-str! data))))))

(defn sse-feed
  "Incremental SSE line splitter. STATE is {:buffer string}; TEXT is the next
   chunk off the wire, cut anywhere. Returns [state' complete-lines]."
  [state text]
  (let [parts (text/lines (str (:buffer state) text))]
    [{:buffer (peek parts)} (pop parts)]))

(defn chunk->delta
  "The delta of one streamed CHUNK, flattened: {:role :content :tool_calls
   :finish_reason :usage} plus reasoning keys, nil-valued keys omitted."
  [chunk]
  (let [choice (first (:choices chunk))
        d      (:delta choice)]
    (cond-> (reasoning-of d)
      (:role d)                 (assoc :role (:role d))
      (:content d)              (assoc :content (:content d))
      (seq (:tool_calls d))     (assoc :tool_calls (mapv tool-call (:tool_calls d)))
      (:finish_reason choice)   (assoc :finish_reason (:finish_reason choice))
      (:usage chunk)            (assoc :usage (:usage chunk))
      (:model chunk)            (assoc :model (:model chunk)))))

(defn- merge-tool-call
  [calls tc]
  (let [idx (or (:index tc) (count calls))
        cur (get calls idx)]
    (assoc calls idx
           (if cur
             (cond-> (update-in cur [:function :arguments] str (get-in tc [:function :arguments]))
               (:id tc)                    (assoc :id (:id tc))
               (get-in tc [:function :name]) (assoc-in [:function :name] (get-in tc [:function :name])))
             (dissoc tc :index)))))

(defn accumulate
  "Fold DELTA into the partial message ACC (start with {}). Content and
   reasoning text concatenate; tool calls merge by :index (kept in a map
   from index to call until `finish`), their argument fragments
   concatenating."
  [acc delta]
  (let [cat (fn [m k] (if-let [v (get delta k)] (update m k str v) m))]
    (cond-> (-> acc (cat :content) (cat :reasoning) (cat :reasoning_content))
      (:role delta)               (assoc :role (:role delta))
      (:reasoning_details delta)  (update :reasoning_details (fnil into []) (:reasoning_details delta))
      (:tool_calls delta)         (update :tool_calls #(reduce merge-tool-call (or % {}) (:tool_calls delta)))
      (:finish_reason delta)      (assoc :finish_reason (:finish_reason delta))
      (:usage delta)              (assoc :usage (:usage delta))
      (:model delta)              (assoc :model (:model delta)))))

(defn finish
  "The MESSAGE an accumulated stream ACC stands for; tool calls in index
   order."
  [acc]
  (cond-> (merge {:role "assistant"} acc)
    (:tool_calls acc) (update :tool_calls #(mapv val (sort-by key %)))))

(defn lines->message
  "Result: the MESSAGE a complete stream of SSE LINES stands for. A chunk
   carrying an error object yields err :llm/api-error."
  [lines]
  (loop [ls (seq lines) acc {}]
    (if-not ls
      (r/ok (finish acc))
      (let [d (sse-data (first ls))]
        (cond
          (= d :done)             (r/ok (finish acc))
          (and (map? d) (:error d)) (r/err :llm/api-error {:detail (:error d)})
          (map? d)                (recur (next ls) (accumulate acc (chunk->delta d)))
          :else                   (recur (next ls) acc))))))
