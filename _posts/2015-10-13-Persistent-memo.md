---
layout: post
title: HaskellのDBライブラリPersistentの使い方でわからないことなどの雑なメモ
date: 2015-10-13
---

- derivingPersistFieldがうまくいかない。その型がnot defined locallyと言われてしまう
 - => dirivePersistentFieldしたい型を別モジュールに分けたらうまくいった。TemplateHaskellの制限っぽい
- http://blog.fujimuradaisuke.com/post/26887032662/haskell-de-json-web-api にあるように、テーブル名の横にjsonと書くだけでToJSON,FromJSONのインスタンスにするのがうまくいかない

- type synonymをPersistFieldにしたいのだけど、type synonymをToJSONなどのインスタンスにする方法がわからない（最終的にpersistentで定義したテーブルの型をToJSON,FromJSONのインスタンスにしたいので） 
- Data.TreeのTree aをPersistFieldにしたいが方法がわからない（できるかどうかもわからない）
- トランザクションについて調べる
- いつの間にかmigrationしなくなっちゃった
 - => actionでエラーになってロールバックされてるだけだったみたい
