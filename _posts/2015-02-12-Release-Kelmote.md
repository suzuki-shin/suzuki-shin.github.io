---
layout: post
title: elmでスライド作成ツール作ってみた
---

昨年末の[Elm Advent Calendar](http//qiita.com/advent-calendar/2014/elm)を見て、[elm](http://elm-lang.org/)に興味を持ち、ちょこちょこ触ってました。で、とりあえず何か作ってみようということで、スライドっぽいウェブページを作るツール（Kelmote）を作って見ました。
それで作ったサンプルは (http://suzuki-shin.github.io/kelmote/)
カーソルキー左右（もしくはクリックorタッチ）でページ送り（戻し）できます。
ソースは (https://github.com/suzuki-shin/kelmote) においてあります。

## elm触った感想

### ちゃんと静的型がついてうれしい
ぼくがAltJSに求める一番のものはこれ。コンパイラが型エラーもちゃんと教えてくれて捗る。

### Haskellっぽいsyntaxうれしい
まあ、Haskell好きなので。でも[HaskellというよりMLじゃんという話も]。(https://twitter.com/shin16s/status/546241498828652544)

### 環境作るのが楽
macではバイナリ持ってきてインストールできた。windowsもそうみたい。
PureScriptとかって結構環境作るのに苦労した覚えがあるんだけど、elmは超らくちん。

### ドキュメントやサンプルコードが結構充実している
web上で編集できるサンプルがたくさんあって勉強が捗る。

### Signal結構難しい
まだ僕ではやりたくてもどうやればいいのかわからないことが結構ある。何かのトリガーで開始したりリセットしたりするタイマーとかどうやって作ればいいのかわからない><超簡単にできそうなのに。
あとフォーム周りがまだいまいちわかってない><

### やっぱりモナド使いたい

### FRPとかVirtualDOMとかelmで入門するのもいいんじゃない？