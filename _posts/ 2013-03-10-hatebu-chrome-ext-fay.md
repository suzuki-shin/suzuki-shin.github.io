---
layout: post
title: 昔作ったはてぶのコメントを表示するchromeの拡張をfay化してみた
---

昔chromeの拡張の練習で作った、はてぶコメントを表示する拡張をfayで作り直してみた。
fayはざっくり説明すると、haskellを書くとjavascriptになるというもの。詳しくは[こちら](https://github.com/faylang/fay/wiki)を。


[ソースコード](https://github.com/suzuki-shin/htbcomment_chrome_ext)

```haskell
{-# LANGUAGE EmptyDataDecls    #-}
module Htbcomment2 (main) where

import Prelude
import FFI
-- import MyPrelude
import JS
-- import ChromeExt

baseUrl :: String
baseUrl = "http://b.hatena.ne.jp/entry/jsonlite/"
cacheHour :: Int
cacheHour = 10
cacheMSec :: Int
cacheMSec = cacheHour * 60 * 60 * 1000;

main :: Fay ()
main = do
  ready $ do
--     select "#dump" >>= append "debug xxx"
    wid <- getWindowId
    chromeTabsGetselected wid $ \tab ->
      getJSON (baseUrl ++ (propStr "url" tab)) $ \e ->
--       getCacheAnd (baseUrl ++ (propStr "url" tab)) $ \e ->
        displayComment (propBookmarks "bookmarks" e)


chromeTabsGetselected :: Int -> (a -> Fay ()) -> Fay ()
chromeTabsGetselected = ffi "chrome.tabs.getSelected(%1, %2)"

getWindowId :: Fay Int
getWindowId = ffi "window.id"


setCache :: String -> a -> Fay ()
setCache url json = localStorageSet url $ show json

-- getCache :: String -> Maybe String
getCache :: String -> Maybe a
getCache url = case localStorageGet url of
  Null -> Nothing
  Nullable cache -> Just $ jsonParse cache
--   cache -> Just $ jsonParse cache

getCacheAnd :: String -> (a -> Fay ()) -> Fay ()
getCacheAnd url f = case getCache url of
   Just entry -> do
     putStrLn "Just"
     putStrLn $ show entry
     putStrLn "----------"
--      putStrLn $ show (propBookmarks "bookmarks" entry)
     f entry
   Nothing -> do
     select "#dump" >>= append "api access..."
     putStrLn "nothing"
     getJSON url $ \entry -> do
       f entry
       setCache url entry


displayComment :: [Bookmark] -> Fay ()
displayComment [] = return ()
displayComment (b:bs) = case (propStr "comment" b) of
  "" -> displayComment bs
  c  -> do
    select "#comments" >>= append (c ++ "<br>")
    displayComment bs

data Tab = Tab {
    active :: Bool
  , url :: String
  , favIconUrl :: String
  , index :: Int
  , title :: String
  , windowId :: Int
  } deriving (Show)

data Bookmark = Bookmark {
    user :: String
  , tags :: [String]
  , timestamp :: String
  , comment :: String
  } deriving (Show)

propStr :: String -> a -> String
propStr = ffi "%2[%1]"

propBookmarks :: String -> a -> [Bookmark]
propBookmarks = ffi "%2[%1]"

jsonParse :: String -> a
jsonParse = ffi "JSON.parse(%1)"
```

### fayで書いた感想
* Haskell(のサブセット)でかけるのはやっぱりうれしい。型もかけるし、もちろん型チェックも入る。
* js側とやりとりするデータの扱い方がよくわからない。たとえばffiや外から受け取ったjsonデータはfay側でどういう型のデータになるのかまだよくわからない。
* js側との文字列のやりとりがややこしい。HaskellではStringはCharのリストなのだけど、jsではそうではないので、そのことによって問題がおきることがあるっぽい。Haskellでは文字列をmapで変換するとかよくやると思うんだけど、fayだとそのときによくわからないエラーになることが時々ある気がする。(これはたぶん僕がまだfayのことをあまりわかっていないせいだけど)
* js側でエラーになったときにデバッグが難しい。fayが吐くjsは普通のjsじゃなくて、読めばすぐわかるようなものじゃないので。
* Fayモナド以外のモナドが使えないのは悲しい。あとApplicativeも使いたいよ。
* Haskellのいろんなライブラリが使えないのは悲しい。コピペですむレベルのものは使えるけど。
* 実はjsでそのまま書いていた方はキャッシュをlocalStorageに保存しているのだけど、fayのほうではlocalStorageから取り出すところがうまくいってなくて、キャッシュの機能は未実装になってる。

こうしてみると、まだ今の僕にとってはfayで書くメリットより、デメリットのほうが大きいような気がする。現状では多分livescriptとかで書いた方が僕は楽だろう。
でも、fayのことをもっとわかっていけば、いずれそれは逆転するのではないかという期待があるのでfayは続けていこうと思う。あとfay自体がまだ未完成の部分も多々あると思うので、これからどんどん使いやすくなっていくんじゃないかという期待もある。なんといってもHaskell界隈の人たちはめちゃくちゃ頭よくてスーパーなので、きっとどんどんよくなっていくと思う。
