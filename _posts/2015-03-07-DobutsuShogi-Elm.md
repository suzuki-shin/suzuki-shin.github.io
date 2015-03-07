---
layout: post
title: elmでどうぶつしょうぎ作ってみた
date: 2015-03-07
---

[Haskellもくもく会](http://haskellmokumoku.connpass.com/event/12619/)や[渋谷 関数型ハッカソン](http://fancs.connpass.com/event/12143/)で、どうぶつしょうぎをelmで作ってて、だいたいできたので公開。


[![スクリーンショット]({{ site.baseurl }}/images/doubutsuShogi.png)](http://suzuki-shin.github.io/doubutsuShogi/)


ソースは[こちら](https://github.com/suzuki-shin/doubutsuShogi)。現在300行ちょっと。結構書き散らかしてるので、もうちょいリファクタしたい。できればボードゲーム用のモジュール切り出したりしたい。

トライルールは未実装。

今後はトライルール実装して、それから、なんちゃってでも良いからAI実装するか、WebSocketとかでネットワーク対戦できるようにするか、、
とりあえずは見た目をもうちょいなんとかするか。

あと、ねこあつめ、癒されます。
![ねこあつめ]({{ site.baseurl }}/images/nekoatsume.jpg)
