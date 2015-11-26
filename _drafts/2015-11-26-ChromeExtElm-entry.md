---
layout: post
title: Chromeの拡張をElmでつくる
date: 2015-11-26
---

これは[Elm Advent Calendar 2015](http://qiita.com/advent-calendar/2015/elm)のための記事です。

[Elm Advent Calendar 2015](http://qiita.com/advent-calendar/2015/elm)に招待してもらったので、最近めっきりElmを書いていなかったのですが、なんとかネタを絞り出し書いてみました。ちなみに実用性はほぼありませんので悪しからず。。。

### つくったもの
[はてぶコメント表示する拡張](https://github.com/suzuki-shin/chrome_ext_elm/)をElmで作りました。Elmのバージョンは0.16です。

### つくりかた
詳細はソースコードを見てもらうとして、作り方をざっくり説明すると、Chrome Platform API（今回使用したのはtabs.queryだけ。あとからtabs.getCurrentを使えばもっと楽だと気づいたのですが、そのままにしてあります）をElmのNative moduleでラップしてElmから使えるようにして、それで取得したtabのurlを使ってelm-httpではてなブックマークAPIにリクエストを送りはてぶ情報を取得しています。

### つまづいたところ
* 最初は、jsだけでなくhtmlもElmで生成しようと思ったのですが、そうするとscriptタグにsrc属性が無いとchromeに怒られてうまく行かなかったので、jsだけ生成して読みこむようにした。
* なんでかわからないけど、chrome APIの返してくるtab情報のfaviconのurlのプロパティ名が`favIconUrl`（favIconの Iが大文字）になっていて、最初取得できずにハマりました。いや、faviconUrl使ってないんですけど、取得できないとJSONのdecodeのところで失敗するのです。https://developer.chrome.com/extensions/tabs#type-Tab
* tab.queryをラップしたTaskとHttp.getのTaskのErrorの方が違うので、最初まとめ方がわからず苦労した。
 * Task.mapErrorという関数があり、それでErrorの型を変換して解決した。

### まとめ
正直、chromeの拡張はjsや他のaltjsで書いた方が楽だと思います。


