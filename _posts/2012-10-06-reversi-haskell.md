---
layout: post
title: Haskellでオセロっぽいの作ってみた
---

細かいルールは調べてないし、実装もしてない。
他の言語ではオセロとか作ったことないから比較はできないけど、Haskellはすごい作りやすいなと思った。まだまだHaskell力が低いからうまく書けていないけど、それでも普通に作ると、短い関数を作ってそれを組み合わせていくようになる(気がする)し、なれるととても読みやすいように思う。
本当はroopの中のMaybeのcaseのあたりが微妙(IOモナドのdoのなかでMaybeモナドをうまく扱えてないような気がしてる)で、もっときれいに書きたかったのだけど、とりあえず完成させることにしたのでそれは後の課題。モナド変換子とかを使うとうまく書けるのかなと思っていて、そのへんを勉強していく予定。
あとdoctestが良い。Pythonで知った時に感動したけど、これくらい手軽にかけるとテスト書いていこうと思える。あとHaskellだと状態とかあんまないし、テストすごい書きやすい。

runhaskell reversi.hsで

```
12345678

________ 1
______X_ 2
____OXO_ 3
___OOX__ 4
___XXX__ 5
________ 6
________ 7
________ 8


O side turn. input x y.
```

こんな感じ。

ソースコードは以下

Board.hs

```haskell
{-# OPTIONS -Wall #-}
module Board (
    Pos
  , Board
  , BoardInfo (BoardInfo, getSize, getBoard)
  , Mark (E, O, X)
  , isOnBoard
  , emptyBoard
  , roop
  , marksPosOf
  , markOf
  , rev
  , renderBoard -- DEBUG
  ) where

import qualified Data.Map as M

data Mark = E | O | X deriving (Eq)
instance Show Mark where
  show E = "_"
  show O = "O"
  show X = "X"

type Pos = (Int, Int)
type Board = M.Map Pos Mark
data BoardInfo = BoardInfo {getSize :: Int, getBoard :: Board} deriving (Show, Eq)
data Result =  Lose | Draw | Win

emptyBoard :: Int -> BoardInfo
emptyBoard boardSize = BoardInfo boardSize $ M.fromList [((x, y) , E) | x <- [1..boardSize], y <- [1..boardSize]]

roop :: BoardInfo
        -> (BoardInfo -> Pos -> Mark -> BoardInfo)
        -> (BoardInfo -> Pos -> Mark -> Bool)
        -> (Board -> Mark -> Bool)
        -> (Board -> Mark -> Bool)
        -> (Board -> Mark -> Bool)
        -> Mark
        -> IO ()
roop boardInfo action canPut checkWin checkDraw checkLose mark = do
  renderBoard boardInfo
  boardInfo' <- turn boardInfo action canPut mark
  case checkFinish (getBoard boardInfo') checkWin checkDraw checkLose mark of
    Just Win -> do
      renderBoard boardInfo'
      putStrLn $ show mark ++ " side win!"
    Just Draw -> do
      renderBoard boardInfo'
      putStrLn "draw"
    Just Lose -> do
      renderBoard boardInfo'
      putStrLn $ show mark ++ " side lose!"
    Nothing -> roop boardInfo' action canPut checkWin checkDraw checkLose (rev mark)


-- | Posが盤上かどうかを返す
-- >>> isOnBoard 5 (1,5)
-- True
-- >>> isOnBoard 5 (1,6)
-- False
-- >>> isOnBoard 5 (6,3)
-- False
-- >>> isOnBoard 5 (0,5)
-- False
isOnBoard :: Int -> Pos -> Bool
isOnBoard size (x, y) = (x >= 1 && x <= size) && (y >= 1 && y <= size)

-- | boardInfoとPosとMarkをとりPos位置にMarkが置けるかをチェックして、おけるならばおいたboardInfoをRight boardInfoで返し、置けないならばLeft errを返す
-- >>> let bi = BoardInfo {getSize = 2, getBoard = M.fromList [((1,1),O),((1,2),X),((2,1),E),((2,2),E)]}
-- >>> let canPut boardInfo pos _ = (isOnBoard (getSize boardInfo) pos) && ((markOf (getBoard boardInfo) pos) == Just E)
-- >>> putMark bi canPut (2,1) O
-- Right (BoardInfo {getSize = 2, getBoard = fromList [((1,1),O),((1,2),X),((2,1),O),((2,2),_)]})
-- >>> putMark bi canPut (1,3) O
-- Left "Can't put there."
-- >>> putMark bi canPut (1,1) O
-- Left "Can't put there."
putMark :: BoardInfo -> (BoardInfo -> Pos -> Mark -> Bool) -> Pos -> Mark
           -> Either String BoardInfo
putMark boardInfo canPut pos mark
  | canPut boardInfo pos mark = Right $ BoardInfo (getSize boardInfo) (M.insert pos mark (getBoard boardInfo))
  | otherwise = Left "Can't put there."

-- | Markを指定して、盤上のそのMarkすべての位置を返す
-- >>> let b = M.fromList [((1,1),O),((1,2),X),((1,3),O),((2,1),E),((2,2),E),((2,3),O),((3,1),X),((3,2),E),((3,3),E)] :: Board
-- >>> marksPosOf b O
-- [(1,1),(1,3),(2,3)]
-- >>> marksPosOf b X
-- [(1,2),(3,1)]
-- >>> marksPosOf b E
-- [(2,1),(2,2),(3,2),(3,3)]
marksPosOf :: Board -> Mark -> [Pos]
marksPosOf board mark = map fst $ filter (\(_, m) -> m == mark) $ M.toList board

-- | 位置を指定して、その位置にあるMarkをMaybe Markで返す
markOf :: Board -> Pos -> Maybe Mark
markOf board pos = M.lookup pos board

-- | 標準入力から座標を入力させて(正しい入力でない場合は正しくなるまで繰り返す)、その座標にマークをおき、何らかの処理をして、
turn :: BoardInfo
        -> (BoardInfo -> Pos -> Mark -> BoardInfo)
        -> (BoardInfo -> Pos -> Mark -> Bool)
        -> Mark
        -> IO BoardInfo
turn boardInfo action canPut mark = do
  putStrLn $ (show mark) ++ " side turn. input x y."
  pos <- inputToPos
  case putMark boardInfo canPut pos mark of
    Right boardInfo' -> return $ action boardInfo' pos mark
    Left err -> do
      putStrLn err
      turn boardInfo action canPut mark
    where
      inputToPos :: IO (Int, Int)
      inputToPos = do
        l <- getLine
        if length (words l) == 2
          then return $ list2ToTuple2 $ map read $ words l
          else inputToPos
      list2ToTuple2 :: [Int] -> (Int, Int)
      list2ToTuple2 [n1, n2] = (n1, n2)
      list2ToTuple2 _ = error "list2ToTuple2 args error"

-- | ボードを描画する
renderBoard :: BoardInfo -> IO ()
renderBoard (BoardInfo size board) = do
--   putStrLn "123\n   "
  mapM_ (putStr . show . (\n -> if n >= 10 then n `mod` 10 else n)) [1..size]
  putStrLn "\n"
  mapM_ renderCol $ M.toList board
  putStrLn "\n"
    where
      renderCol :: (Pos, Mark) -> IO ()
      renderCol ((x,y), mark)
        | y == size = putStrLn $ show mark ++ " " ++ show x
        | otherwise = putStr $ show mark

rev :: Mark -> Mark
rev O = X
rev X = O
rev E = E

-- | ゲームが終了条件を満たしているかをチェックし、満たしていればJust結果を、満たしていなければNothingを返す
checkFinish :: Board
               -> (Board -> Mark -> Bool)
               -> (Board -> Mark -> Bool)
               -> (Board -> Mark -> Bool)
               -> Mark
               -> Maybe Result
checkFinish board win draw lose mark
  | win board mark = Just Win
  | draw board mark = Just Draw
  | lose board mark = Just Lose
  | otherwise = Nothing
```

reversi.hs

```haskell
{-# OPTIONS -Wall #-}
import Board
import qualified Data.Map as M
import Data.Maybe
import Control.Monad
-- import Debug.Trace

boardSize :: Int
boardSize = 8

data Direct = LeftUpD | UpD | RightUpD
            | LeftD | RightD
            | LeftDownD | DownD | RightDownD
            deriving (Show, Eq)

-- | 指定PosのDirect方向の隣のPosを返す
-- >>> neighbor (1,1) LeftDownD
-- (0,2)
-- >>> neighbor (1,1) UpD
-- (1,0)
-- >>> neighbor (1,1) RightUpD
-- (2,0)
-- >>> neighbor (1,1) LeftD
-- (0,1)
-- >>> neighbor (1,1) RightD
-- (2,1)
-- >>> neighbor (1,1) LeftDownD
-- (0,2)
-- >>> neighbor (1,1) DownD
-- (1,2)
-- >>> neighbor (1,1) RightDownD
-- (2,2)
neighbor :: Pos -> Direct -> Pos
neighbor (x, y) dir
  | dir == LeftUpD    = (x - 1, y - 1)
  | dir == UpD        = (x, y - 1)
  | dir == RightUpD   = (x + 1, y - 1)
  | dir == LeftD      = (x - 1, y)
  | dir == RightD     = (x + 1, y)
  | dir == LeftDownD  = (x - 1, y + 1)
  | dir == DownD      = (x, y + 1)
  | dir == RightDownD = (x + 1, y + 1)
neighbor _ _ = error "bad args"

allDirections :: [Direct]
allDirections = [ LeftUpD, UpD , RightUpD
                , LeftD, RightD
                , LeftDownD, DownD, RightDownD]

isFinished :: Board -> Bool
isFinished board = length (marksPosOf board E) == 0

checkWin :: Board -> Mark -> Bool
checkWin board mark = isFinished board && length (marksPosOf board mark) > length (marksPosOf board (rev mark))

checkDraw :: Board -> Mark -> Bool
checkDraw board mark = isFinished board && length (marksPosOf board mark) == length (marksPosOf board (rev mark))

checkLose :: Board -> Mark -> Bool
checkLose board mark = isFinished board && not (checkWin board mark) && not (checkDraw board mark)

-- | 指定した位置から指定した方向の列の(Pos,Mark)のリストを返す
-- >>> let bSize = 4 :: Int
-- >>> let bi = BoardInfo bSize $ M.insert (3,3) O $ M.insert (3,2) X $ M.insert (2,3) X $ M.insert (2,2) O (getBoard (emptyBoard bSize))
-- >>> lineOfDirection bi (3,3) UpD
-- [((3,2),X),((3,1),_)]
-- >>> lineOfDirection bi (3,3) LeftUpD
-- [((2,2),O),((1,1),_)]
-- >>> lineOfDirection bi (4,4) LeftUpD
-- [((3,3),O),((2,2),O),((1,1),_)]
-- >>> lineOfDirection bi (4,3) LeftUpD
-- [((3,2),X),((2,1),_)]
-- >>> lineOfDirection bi (4,3) RightD
-- []
lineOfDirection :: BoardInfo -> Pos -> Direct -> [(Pos, Mark)]
lineOfDirection boardInfo pos dir
  | not (isOnBoard (getSize boardInfo) pos) || not (isOnBoard (getSize boardInfo) nextPos) || isNothing markOfNextPos = []
  | otherwise = (nextPos, fromJust markOfNextPos) : (lineOfDirection boardInfo nextPos dir)
  where
    nextPos = neighbor pos dir
    markOfNextPos = markOf (getBoard boardInfo) nextPos

-- | その(line::[(Pos,Mark)])は指定したmarkで挟めるlineか？はさめるならそのlineを、はさめないなら[]を返す
-- >>> clippableLine O [((2,1),X),((3,1),X),((4,1),O)]
-- [(2,1),(3,1)]
-- >>> clippableLine O [((2,1),O),((3,1),X),((4,1),O)]
-- []
-- >>> clippableLine O [((2,1),X),((3,1),X),((4,1),E)]
-- []
clippableLine :: Mark -> [(Pos, Mark)] -> [Pos]
clippableLine mark posMarks
  | length body < length posMarks && snd (posMarks!!(length body)) == mark = body
  | otherwise = []
  where
    body = body' mark posMarks
    body' mark' (pm:pms)
      | snd pm == rev mark' = (fst pm) : (body' mark' pms)
      | otherwise = []
    body' _ _ = []

-- | そこにmarkを置いた場合あいてのmarkを挟めるか
-- >>> let bSize = 4 :: Int
-- >>> let bi = BoardInfo bSize $ M.insert (3,3) O $ M.insert (3,2) X $ M.insert (2,3) X $ M.insert (2,2) O (getBoard (emptyBoard bSize))
canClip :: BoardInfo -> Pos -> Mark -> Bool
canClip bi p m
  | not (any (\d -> clippableLine m (lineOfDirection bi p d) /= []) allDirections) = False
--   | filter (\d -> clippableLine m (lineOfDirection bi p d) /= []) allDirections == [] = False
  | otherwise = True

-- | そこにそのマークを置けるかどうか
-- >>> let bi = initBoard 4 [((2,2),O), ((2,3),X),((3,2),X), ((3,3),O)]
-- >>> canPut bi (1,3) O
-- True
-- >>> canPut bi (1,2) X
-- True
-- >>> canPut bi (1,3) X
-- False
canPut :: BoardInfo -> Pos -> Mark -> Bool
-- canPut boardInfo pos mark  =    trace("is not OnBoard") ((isOnBoard (getSize boardInfo) pos))
--                              && trace("not E") (((markOf (getBoard boardInfo) pos) == Just E))
--                              && trace("can not clip") (canClip boardInfo pos mark)
canPut boardInfo pos mark  =    isOnBoard (getSize boardInfo) pos
                             && (markOf (getBoard boardInfo) pos) == Just E
                             && canClip boardInfo pos mark

-- | 指定した位置のmarkをひっくり返す
turnBack :: Board -> [Pos] -> Board
turnBack board ps = foldl (\b p -> M.insert p (revMark p) b) board ps
  where
    revMark :: Pos -> Mark
    revMark p = rev $ fromJust $ M.lookup p board

-- | markをおいたあとにboardInfoがどのように変更されるか
action :: BoardInfo -> Pos -> Mark  -> BoardInfo
action bi pos mark = BoardInfo bSize (turnBack b clippablePoses)
  where
    b = getBoard bi
    bSize = getSize bi
    dirs = filter (\d -> clippableLine mark (lineOfDirection bi pos d) /= []) allDirections
    clippablePoses = join $ map (\d -> clippableLine mark (lineOfDirection bi pos d)) dirs

initBoard :: Int -> [(Pos, Mark)] -> BoardInfo
initBoard bSize posMarks = BoardInfo bSize $ foldl (\b (pos, mark) -> M.insert pos mark b) (getBoard (emptyBoard bSize)) posMarks

main :: IO ()
main = do
  let boardInfo = initBoard boardSize [((center,center),O), ((center,center+1),X),((center+1,center),X), ((center+1,center+1),O)]
  roop boardInfo action canPut checkWin checkDraw checkLose O
    where
      center = boardSize `div` 2
```

そのうち気が向いたらfayとかそういうのでjsにしてブラウザで動くようにしようかな。できるかどうかわからないけど。
