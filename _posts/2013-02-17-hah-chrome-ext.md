---
layout: post
title: hit a hintやタブ切り替えなどの機能をもつchromeの拡張を作った
---

[hah](https://chrome.google.com/webstore/detail/hah/ikljpmlpcmlghjponhkhibgbfhjgjbki)
まだスクリーンショットもアイコンもなくてアルファバージョンだけど、使えることは使える。

ソースコードは[これ](https://github.com/suzuki-shin/hah_chrome_ext)

## 機能
いまのところ、機能は以下のもの

### hit a hint
eでhit a hintを開始、表示される2文字のアルファベットをタイプすると、そのリンクをクリックできる。

### タブ切り替え(履歴やブックマークもあるよ)
;でウィンドウが開き、そこに現在開いているタブ、それから履歴やブックマックのリストが表示される。
タイプするとその文字をタイトルやURLに含むものだけに絞り込まれていく。
上下にカーソルを動かすことができ、エンターを押すとタブならそのタブに切り替え、履歴やブックマークなら新たなタブでそのURLを開く。

### 最初のフォームにフォーカスする
fで最初のフォームにフォーカスが移る

## 動機
もともと作った動機は、chromeで使いやすいhit a hintの拡張がなかったので、自分で作ろうと思ったというところ。
本当はfayで作ろうかと思っていたのだけど、ちょっと触ってみてすぐにはできそうになかったので、livescriptで作った。

##  今後
まだかなり未完成なのでバグもたくさんあるし、使いづらいところもあると思うけど、ぼちぼち修正と機能追加をしていくつもり。