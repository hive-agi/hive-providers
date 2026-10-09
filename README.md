# hive-providers

OAuth 2.0 and OpenAI-compatible LLM providers for Clojure harnesses, in one
self-contained `.cljc` library that reads on every major Clojure dialect.

- **OAuth**: PKCE authorization-code flow (RFC 7636), refresh and
  client-credentials grants (RFC 6749), device authorization (RFC 8628),
  JWT claim reading, a token authority that decides reuse vs refresh, and the
  credential files of Claude Code, the codex CLI and Kimi Code.
- **OpenAI-compatible chat**: a registry of 30+ providers with their quirks
  (auth header schemes, URL templates, field allowlists, tool-incompatible
  models, reasoning fields), request building, response and SSE stream
  parsing with tool-call reassembly, retry policy, and an `IChatProvider`
  protocol for harnesses to depend on.

JSON, base64, URL encoding, UTF-8 and SHA-256 are portable Clojure in
`src/`. The two dependencies are portable `.cljc` as well:

- **hive-dsl**: Results are hive-dsl Results (`{:ok v}` / `{:error cat ...}`,
  railway-oriented), and the closed sums in the flows are hive-dsl ADTs
  (`oauth.authority/Decision`, `oauth.device/PollOutcome`, `openai.retry/Step`),
  matched exhaustively with `adt-case`.
- **malli**: `hive-providers.schema` describes every boundary value (HTTP
  request/response, OAuth client and token, PKCE bundle, device authorization,
  registry entry, chat params, message). `hive-providers.contracts` registers
  function schemas for the public API, so `malli.instrument` can check a
  running harness.

**hive-system is provided scope.** Only `hive-providers.host.hive-system`
needs it, and it is not in `:deps`: a harness that already runs hive-system
(hive-agent, hive-mcp) gets `pass:`-backed secret refs and hive-system's file
lock for the single-flight refresh. Every other namespace loads without it.
Clojure loads per namespace but resolves dependencies per artifact, so keeping
hive-system out of `:deps` is what keeps datahike, tink and the rest out of
every consumer's classpath.

## Design: effects as data

No flow performs I/O. Each one is a *program*: a value that is either done or
asks the host for one operation and says how to continue
(`hive-providers.effect`). A harness runs a program with a map of handlers:

```clojure
(require '[hive-providers.effect :as fx]
         '[hive-providers.host.jvm :as jvm]
         '[hive-providers.openai.chat :as chat]
         '[hive-providers.openai.registry :as reg])

(def provider (chat/provider reg/seed (jvm/handlers)))

(chat/chat provider {:model "groq:llama-3.3-70b-versatile"
                     :messages [{:role "user" :content "hello"}]})
;; => {:ok {:role "assistant" :content "..." :usage {...} :model "..."}}
```

Ops: `:http/request`, `:clock/now-ms`, `:entropy/bytes`, `:sleep/ms`,
`:env/get`, `:secret/get`, `:store/load`, `:store/save`. A registry entry
may name `:api-key-ref` (`"pass:Venice/key"`, `"env:X"`); the secret store
answers before the environment, so a stale shell variable cannot shadow the
canonical key. The same ports exist as protocols
(`hive-providers.ports`) with stub records for tests, so a harness depends
on abstractions and injects concretions (DIP). Failures are values:
`{:ok v}` or `{:error :category ...}`, the same shape as hive-dsl.

Host adapters:

| Host | Namespace | Notes |
|---|---|---|
| JVM, Babashka | `hive-providers.host.jvm` | `java.net.http`, `SecureRandom`, atomic owner-only JSON files, `single-flight` per-account lock (plus OS file lock) |
| Node.js 18+ | `hive-providers.host.node` | `fetch` with chunked SSE reading, `crypto.randomBytes`, async handlers for `fx/run-async` |
| JVM + hive-system | `hive-providers.host.hive-system` | provided scope: `pass:` secrets via hive-system's registry, single flight under hive-system's `fs-lock!` |
| any other | your handlers | a map of op to fn; nothing else is required |

## OAuth example: share Claude Code's login

```clojure
(require '[hive-providers.oauth.authority :as auth]
         '[hive-providers.oauth.clients :as clients]
         '[hive-providers.oauth.vendor :as vendor]
         '[hive-providers.result :as r])

(def h (jvm/handlers {:stores {:claude (str (jvm/home-path ".claude" ".credentials.json"))}}))

(jvm/single-flight h :claude
  (auth/access-program (r/value (clients/client :anthropic/claude-ai)) :claude
                       {:decode vendor/claude->token
                        :encode vendor/claude-file-encode}))
;; => {:ok {:access-token "..." :expires-at-ms ...}}
```

The token is refreshed only when it is near expiry, written back in Claude
Code's own file shape, and concurrent callers on one account refresh once.

## Dialects

`bin/test-dialects` runs the suite on every runtime it finds.

| Dialect | Runtime | Status |
|---|---|---|
| Clojure (JVM) | `clojure` | full suite + JVM host |
| Babashka | `bb` | full suite + JVM host |
| ClojureScript | `clojure -M:cljs-test` on Node | full suite + Node host (real `fetch` stream) |
| ClojureWasm | `cljw` | full portable suite |
| clojurust | `cljrs` | full portable suite |
| Basilisp | `basilisp test` (with pytest) | full portable suite |
| ClojureElisp | `clel` + Emacs | codecs, SHA-256, PKCE, JSON writing verified in Emacs; see below |
| ClojureCLR, ClojureDart | untested | no runtime here; the sources use no JVM interop outside `platform` and `host/` |

`hive-providers.platform` is the only namespace that branches per dialect.
Everything else is plain `clojure.core`, written to the common subset the
runtimes above agree on: binary bit ops (ClojureWasm), no `recur` inside
`case` (Basilisp), no regex literals (`hive-providers.text` splits and
validates instead), booleans tested with `boolean?` (clojurust), no calls
through `let`-bound fns and no `t` local (ClojureElisp).

ClojureElisp has no reader conditionals, so `bin/emit-dialect cljel`
resolves them into `target/cljel/**.cljel`, compiles each namespace with
`clel`, and loads the result in a batch Emacs with
`test-cljel/hive-providers-cljel-prelude.el` (aliases for clojure.core fns the
clel runtime has but its compiler does not map yet). Known clel gaps, reported
by the smoke run rather than hidden: keyword namespaces are dropped, a core
fn passed as a value compiles to a variable reference, keyword invocation
fails in some positions, and `openai.chat` does not compile.

## Development

```sh
bb test                      # fastest loop
clojure -M:test              # JVM
bin/test-dialects            # every installed dialect
bin/test-dialects cljw cljrs # some of them
```

## License

MIT. See `LICENSE`.
