---
layout: post
title: elmで作ったどうぶつしょうぎをmilkcocoa.jsで通信対戦できるようにした
date: 2015-03-22
---

先日、どうぶつしょうぎをelmで作ったのをtwitterでつぶやいたら@\_sgtnさんがmilkcocoa.jsというのを教えてくれたので、Portsを使って、milkcocoa.jsで通信対戦できるようにしてみた。
<blockquote class="twitter-tweet" lang="ja"><p><a href="https://twitter.com/shin16s">@shin16s</a> コード読みませていただきました。elm portsでmilkcocoa.jsを使えるようにしてみたりしませんか？(強引w)</p>&mdash; shogochiai(˘ω˘) (@_sgtn) </blockquote>
https://twitter.com/_sgtn/status/574401450060570624


http://suzuki-shin.github.io/doubutsuShogi?hogehoge123 のように適当なパラメタをつけて、そのURLにアクセスすると盤を共有できて、駒を動かすこともできる。

![どうぶつしょうぎ通信対戦]({{ site.baseurl }}/images/doubutsuShogi-milkcocoa.png)

[milkcocoa.js](https://mlkcca.com/)知らなかったけど、便利ですね。使うの超簡単だし。
気が向いたらnative moduleにしてelmから使えるようにしてみようかな。
