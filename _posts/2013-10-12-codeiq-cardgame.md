---
layout: post
title: カードゲームの役を判定する
---

CodeIQの問題に挑戦して、出題者の方からフィードバックを頂きました。
もうブログ等に書いても良いとこの事だったので、書きます。
挑戦したのは@Nabetani 鍋谷 武典さんからのゲームの問題で[「カードゲームの役を判定する」](https://codeiq.jp/ace/nabetani_takenori/q476)というものです。
言語の指定がなかったので、ぼくはHaskellで解きました。

```haskell
{-# OPTIONS_GHC -Wall #-}
import Data.List
import Data.List.Split
import Data.Maybe
import Control.Applicative

data WinningHand = An | Sdt | Dt | Sctp | Ctp | Stp | Tp deriving (Show, Eq, Ord)
data Suit = S | H | D | C deriving (Show, Eq, Ord)
type Rank = Int
type Card = (Suit, Rank)

-- 手札の中のランクのリストを返す
ranks :: [Card] -> [Rank]
ranks = map snd

-- 手札の中のスートのリストを返す
suits :: [Card] -> [Suit]
suits = map fst

-- 入力の一行をカードのリストに変換して返す
dataToCards :: String -> [Card]
dataToCards input = map (\s -> (charToSuit (head s), strToRank (tail s))) $ splitOn "," input

charToSuit :: Char -> Suit
charToSuit 'S' = S
charToSuit 'H' = H
charToSuit 'D' = D
charToSuit 'C' = C
charToSuit _ = error "xxx"

strToRank :: String -> Rank
strToRank "A" = 1
strToRank "2" = 2
strToRank "3" = 3
strToRank "4" = 4
strToRank "5" = 5
strToRank "6" = 6
strToRank "7" = 7
strToRank "8" = 8
strToRank "9" = 9
strToRank "10" = 10
strToRank "J" = 11
strToRank "Q" = 12
strToRank "K" = 13
strToRank _ = error "xxx"

groupedRanks :: [Card] -> [[Rank]]
groupedRanks = group . sort . ranks

groupedSuits :: [Card] -> [[Suit]]
groupedSuits = group . sort . suits

-- 手札の中に含まれるランクの種類の数を返す
lengthRankKind :: [Card] -> Int
lengthRankKind = length . groupedRanks

-- 手札の中に含まれるスートの種類の数を返す
lengthSuitKind :: [Card] -> Int
lengthSuitKind = length . groupedSuits

-- ランクのリストが連続しているかどうかを返す
isContinuous :: [Rank] -> Bool
isContinuous [] = True
isContinuous (_:[]) = True
isContinuous (r:rs) = (isCont r (head rs) && isContinuous rs) || (r == 1 && last rs == 13 && isContinuous (rs ++ [r]))
  where
    isCont 13 1 = True
    isCont a b = succ a == b

-- 指定した役をもっているか
haveWiningHand :: WinningHand -> [Card] -> Bool
haveWiningHand An   cards = lengthRankKind cards == 2 && (sort $ map length (groupedRanks cards)) == [2,4]
haveWiningHand Sdt  cards = lengthRankKind cards == 2 && map length (groupedSuits cards) == [2,2,2]
haveWiningHand Dt   cards = lengthRankKind cards == 2
haveWiningHand Sctp cards = lengthRankKind cards == 3 && map length (groupedSuits cards) == [3,3] && map length (groupedRanks cards) == [2,2,2] && (isContinuous $ map head (groupedRanks cards))
haveWiningHand Ctp  cards = lengthRankKind cards == 3 && map length (groupedRanks cards) == [2,2,2] && (isContinuous $ map head (groupedRanks cards))
haveWiningHand Stp  cards = lengthRankKind cards == 3 && map length (groupedSuits cards) == [3,3]
haveWiningHand Tp   cards = lengthRankKind cards == 3 && map length (groupedRanks cards) == [2,2,2]

-- どの役か？
winingHand :: [Card] -> Maybe WinningHand
winingHand cards = listToMaybe $ filter (\wh -> haveWiningHand wh cards) [An, Sdt, Dt, Sctp, Ctp, Stp, Tp]

main :: IO ()
main = do
  ls <- lines <$> readFile "data.txt"
  let cardsList = map dataToCards ls
  mapM_ print $ map (\x -> (head x, length x)) $ group $ sort $ catMaybes $ map winingHand cardsList
```

[CodeIQ_id=54590](https://gist.github.com/suzuki-shin/6950587)

haveWiningHandを含め、急いで書いたのであれな部分が色々あると思いますので、暇を見つけてリファクタしたいと思います。

最後に、面白い問題、そしてフィードバックありがとうございました。
