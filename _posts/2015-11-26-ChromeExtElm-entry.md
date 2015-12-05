---
layout: post
title: Chromeの拡張をElmでつくる
date: 2015-11-26
---

これは[Elm Advent Calendar 2015](http://qiita.com/advent-calendar/2015/elm)のための記事です。

[Elm Advent Calendar 2015](http://qiita.com/advent-calendar/2015/elm)に招待してもらったので、最近めっきりElmを書いていなかったのですが、なんとかネタを絞り出し書いてみました。実用性はほぼありませんので悪しからず。。。
ちなみに僕がelmを書き始めたのは、[Elm Advent Calendar 2014](http://qiita.com/advent-calendar/2014/elm)を見て興味をもったのがきっかけでした。
その恩返しというわけではないですが、また他の誰かがAdvent Calendarを見てelmやその他の新しい技術に興味をもってくれたら良いなと思っています。

### つくったもの
[はてぶコメント表示する拡張](https://github.com/suzuki-shin/chrome_ext_elm/)をElmで作りました。Elmのバージョンは0.16です。

![SS](https://raw.githubusercontent.com/suzuki-shin/chrome_ext_elm/master/chrome_ext_elm_screen_shot.png)

### つくりかた
詳細は[ソースコード](https://github.com/suzuki-shin/chrome_ext_elm/)を見てもらうとして、作り方をざっくり説明すると、[Chrome Platform API](https://developer.chrome.com/extensions/api_index)（今回使用したのはtabs.queryだけ。あとからtabs.getCurrentを使えばもっと楽だと気づいたのですが、そのままにしてあります）をElmのNative moduleでラップしてElmから使えるようにして、それで取得したtabのurlを使ってelm-httpではてなブックマークAPIにリクエストを送りはてぶ情報を取得しています。
Native moduleの作り方は[ElmのNative moduleを書く](http://qiita.com/philopon/items/cbc2066242bac6f66af0)を参考にしながらライブラリのソースを読んだりして勉強しました。

### つまづいたところ
* 最初は、jsだけでなくhtmlもElmで生成しようと思ったのですが、そうするとscriptタグにsrc属性が無いとchromeに怒られてうまく行かなかったので、jsだけ生成して読みこむようにした。
* なんでかわからないけど、chrome APIの返してくるtab情報のfaviconのurlのプロパティ名が`favIconUrl`（favIconの Iが大文字）になっていて、最初取得できずにハマりました。いや、faviconUrl使ってないんですけど、取得できないとJSONのdecodeのところで失敗するのです。https://developer.chrome.com/extensions/tabs#type-Tab
* tab.queryをラップしたTaskとHttp.getのTaskのErrorの方が違うので、最初まとめ方がわからず苦労した。
 * Task.mapErrorという関数があり、それでErrorの型を変換して解決した。

### まとめ
正直、chromeの拡張はjsや他のaltjsで書いた方が楽だと思います。ただ、Elmは独自の文化を持っていて、学びの多い言語だと思いますので、興味があればやって損はないと思います。

あと、過去にElmで作ったものについても書いてます。もし良かったら見てみてください。

* [elmで作ったどうぶつしょうぎをmilkcocoa.jsで通信対戦できるようにした](http://suzuki-shin.github.io/Release-doubutsuShogi-Elm-Milkcocoa/)
* [elmでスライド作成ツール作ってみた](http://suzuki-shin.github.io/Release-Kelmote/)

