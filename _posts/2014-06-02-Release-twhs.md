---
layout: post
title: コマンドラインでTwitterにPOSTできるアプリを作った
date: 2014-06-02
---
[前回のHaskellもくもく会](http://haskellmokumoku.connpass.com/event/6063/)のときに作ってたやつがだいたいできたかなという感じなのでブログに書いておく。

![スクリーンショット]({{ site.baseurl }}/images/スクリーンショット 2014-06-02 13.44.20.png)

[リポジトリ](https://github.com/suzuki-shin/twhs)

これをつくり始めたのは、twitterに自分用のメモ的な意味でPOSTをしたいことが僕はよくあって、そのときに
つぶやきたい => twitter（web、アプリ）を開く => timelineが見える => timelineを読み込んでしまう => 15分経つ => 何をつぶやこうとしていたか忘れる 
みたいなことがよくあって、ただつぶやくだけのアプリをつくろうと思ったことがきっかけ。しかし、結局timelineを取得する機能もつけちゃった。というか、本当はrubyでつくられたtwというアプリがあって、それを使ってたんだけど、自分でもhaskellで作ってみたかったというだけ

言語はhaskellで、twitter-conduitというライブラリをつかって作りました。というかtwitter-conduitのサンプルコードだけでだいたい用は足りてしまったという。。

問題なければそのうちhackageにアップしちゃおうかと思ってる。
