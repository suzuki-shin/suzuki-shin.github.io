---
layout: post
title: mapM (\_ -> [O, X])
---

```
ほげ
ふが
あべし
ひでぶ
```

という入力をしたら

```
ほげ	ふが	あべし	ひでぶ
O	O	O	O
O	O	O	X
O	O	X	O
O	X	O	O
X	O	O	O
O	O	X	X
O	X	X	O
・・・
```

みたいなOXの全組み合わせのマトリクスを出力するというツールを作るというお題があったのでHaskellで作ってみた

```haskell
{-# OPTIONS -Wall #-}
import Control.Applicative
import Data.List

data State = O | X deriving Show

main :: IO ()
main = do
  items <- lines <$> getContents
  printHeader items
  printMatrix $ matrix items

-- | 入力項目のリストに対するO、Xのマトリクスを返す
-- >>> matrix ["hoge","fuga","foo"]
-- [[O,O,O],[O,O,X],[O,X,O],[O,X,X],[X,O,O],[X,O,X],[X,X,O],[X,X,X]]
-- >>> matrix []
-- [[]]
matrix :: [String] -> [[State]]
matrix = mapM (\_ -> [O, X])

-- | Matrixデータを表形式で出力する
printMatrix :: [[State]] -> IO ()
printMatrix (col:cols) = do
  putStr $ listToTsv $ map show col
  putStrLn ""
  printMatrix cols
printMatrix [] = return ()

-- | ヘッダを出力する
printHeader :: [String] -> IO ()
printHeader items = do
  putStr $ listToTsv items
  putStrLn ""

-- | 文字列のリストをタブ区切りの文字列にする
-- >>> listToTsv ["hoge","fuga","bar"]
-- "hoge\tfuga\tbar"
listToTsv :: [String] -> String
listToTsv items = intercalate "\t" items
```

結果

```
runhaskell matrix.hs < test.txt
ほげ	ふが	あべし	ひでぶ
O	O	O	O
O	O	O	X
O	O	X	O
O	O	X	X
O	X	O	O
O	X	O	X
O	X	X	O
O	X	X	X
X	O	O	O
X	O	O	X
X	O	X	O
X	O	X	X
X	X	O	O
X	X	O	X
X	X	X	O
X	X	X	X
```

matrix関数のところがごにょごにょしてたらこんな簡単になった。Haskellすげーなと。

```haskell
-- | 入力項目のリストに対するO、Xのマトリクスを返す
-- >>> matrix ["hoge","fuga","foo"]
-- [[O,O,O],[O,O,X],[O,X,O],[O,X,X],[X,O,O],[X,O,X],[X,X,O],[X,X,X]]
-- >>> matrix []
-- [[]]
matrix :: [String] -> [[State]]
matrix = mapM (\_ -> [O, X])
```
