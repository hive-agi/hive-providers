(ns hive-providers.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-providers.codec.base64 :as b64]
            [hive-providers.codec.json :as json]
            [hive-providers.codec.url :as url]
            [hive-providers.codec.utf8 :as utf8]
            [hive-providers.crypto.sha256 :as sha]
            [hive-providers.effect :as fx]
            [hive-providers.result :as r]
            [hive-providers.text :as text]))

(deftest utf8-round-trip
  (is (= [104 195 169 108 108 111] (utf8/encode "h\u00e9llo")))
  (is (= "h\u00e9llo" (utf8/decode [104 195 169 108 108 111])))
  (testing "astral code point"
    (let [s (utf8/code-point->str 0x1F600)]
      (is (= [240 159 152 128] (utf8/encode s)))
      (is (= s (utf8/decode [240 159 152 128]))))))

(deftest base64-vectors
  (testing "RFC 4648 section 10"
    (is (= "" (b64/encode-str "")))
    (is (= "Zg==" (b64/encode-str "f")))
    (is (= "Zm8=" (b64/encode-str "fo")))
    (is (= "Zm9v" (b64/encode-str "foo")))
    (is (= "Zm9vYg==" (b64/encode-str "foob")))
    (is (= "Zm9vYmE=" (b64/encode-str "fooba")))
    (is (= "Zm9vYmFy" (b64/encode-str "foobar"))))
  (is (= "foobar" (b64/decode-str "Zm9vYmFy")))
  (is (= "fooba" (b64/decode-str "Zm9vYmE")))
  (is (= "-_8" (b64/encode-url [251 255])))
  (is (= [251 255] (b64/decode "-_8")))
  (is (nil? (b64/decode "a*b"))))

(deftest url-encoding
  (is (= "a%20b%2Bc%26d%3D%C3%A9~-._" (url/encode "a b+c&d=\u00e9~-._")))
  (is (= "a b+c&d=\u00e9" (url/decode "a%20b%2Bc%26d%3D%C3%A9")))
  (is (= "grant_type=refresh_token&refresh_token=a%2Fb"
         (url/form-encode [[:grant_type "refresh_token"] [:refresh_token "a/b"] [:skip nil]])))
  (is (= {"code" "x y" "state" "s"} (url/parse-query "?code=x+y&state=s")))
  (is (= "code=1&state=2" (url/query-of "http://localhost:1/callback?code=1&state=2#frag")))
  (is (= "https://a/b?x=1&y=2" (url/with-query "https://a/b?x=1" {:y 2}))))

(deftest json-codec
  (is (= {:a 1 :b [true false nil] :c "x\"y" :d 1.5 :e {:f -2}}
         (json/read-str! "{\"a\":1,\"b\":[true,false,null],\"c\":\"x\\\"y\",\"d\":1.5,\"e\":{\"f\":-2}}")))
  (is (= "\u00e9\n" (json/read-str! "\"\\u00e9\\n\"")))
  (is (= (utf8/code-point->str 0x1F600) (json/read-str! "\"\\ud83d\\ude00\"")))
  (is (= {"a" 1} (json/read-str! "{\"a\":1}" {:key-fn identity})))
  (is (r/err? (json/read-str "{\"a\":")))
  (is (r/err? (json/read-str "[1,]")))
  (is (r/err? (json/read-str "1 2")))
  (testing "map iteration order is host-defined, so compare single-key maps"
    (is (= "{\"d\":\"q\\\"\\n\"}" (json/write-str {:d "q\"\n"})))
    (is (= "[1,null,true,\"s\"]" (json/write-str [1 nil true :s]))))
  (is (= "{\"ns/k\":\"v\"}" (json/write-str {:ns/k :v})))
  (let [v {:model "m" :messages [{:role "user" :content "h\u00e9 \t"}] :n 3 :f 0.25 :z nil}]
    (is (= v (json/read-str! (json/write-str v))))))

(deftest sha256-vectors
  (is (= "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
         (sha/hex-digest "")))
  (is (= "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
         (sha/hex-digest "abc")))
  (is (= "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
         (sha/hex-digest "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))))

(deftest effect-programs
  (let [prog (fx/bind (fx/perform :clock/now-ms)
                      (fn [t] (fx/fmap (fx/perform :env/get {:name "X"})
                                       (fn [x] [t x]))))]
    (is (= [5 "v"] (fx/run-sync {:clock/now-ms (fn [_] 5)
                                 :env/get (fn [{:keys [name]}] (when (= name "X") "v"))}
                                prog)))
    (is (= :effect/unhandled (:error (fx/run-sync {} prog))))
    (let [out (atom nil)]
      (fx/run-async (fx/sync->async-handlers {:clock/now-ms (fn [_] 1)
                                              :env/get (fn [_] nil)})
                    prog #(reset! out %))
      (is (= [1 nil] @out)))))

(deftest regex-free-text
  (is (= ["a" "" "b" ""] (text/split "a,,b," ",")))
  (is (= ["abc"] (text/split "abc" ",")))
  (is (= ["a" "b=c"] (text/split-first "a=b=c" "=")))
  (is (= ["x" nil] (text/split-first "x" "=")))
  (is (= ["a" "b" "c"] (text/words "  a\tb \n c ")))
  (is (= ["l1" "l2" ""] (text/lines "l1\r\nl2\n")))
  (is (text/digits? "042"))
  (is (not (text/digits? "4a")))
  (is (not (text/digits? "")))
  (is (every? text/json-number? ["0" "-0" "12" "1.5" "-1.5e10" "2E-3" "1e+2"]))
  (is (not-any? text/json-number? ["01" "1." ".5" "-" "1e" "+1" "1.5.2" ""])))
