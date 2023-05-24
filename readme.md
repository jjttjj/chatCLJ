# chatCLJ

A clojure/clojurescript GUI for [OpenAI's chat completion api](https://platform.openai.com/docs/guides/chat). Mainly just a demo app, for me to experiment with [hyperfiddle/electric](https://github.com/hyperfiddle/electric) and [Datomic Pro](https://docs.datomic.com/pro/) and start doing some data modelling for LLM tools.

Much of it is based off the [electric starter app](https://github.com/hyperfiddle/electric-starter-app).

Work in progress, there are rough edges and a lot to be improved. Use at your own risk.

# Usage

Set `OPENAI_API_KEY` to your api key.

```
git clone https://github.com/jjttjj/chatCLJ
cd chatCLJ
clj -X:dev chatclj.system/start
```
