module Lib
    ( someFunc
    ) where

import Control.Monad
import Debug.Trace
import Data.Set as Set
import Control.Parallel.Strategies
import Text.Printf

someFunc :: IO ()
someFunc =
    putStrLn $ show solution

initial :: Tuple
initial =
    ( 1
    , 2, 3
    , 4, 5, 6 )

match :: Tuple -> Bool
match
    (   2
    ,  4, 1
    , 5, 6, 3 )
    = True
match _ = False

solution :: [St]
solution = take 1 $ match_filter $ all_combinations
  where
    match_filter = Prelude.filter match_state
    match_state (St tup _) = match tup

all_combinations :: [St]
all_combinations =
    concat $ iterate (go . dedup) [mkState initial]
  where
      go :: [St] -> [St]
      go xs = do
          x <- xs
          f <- all_trans
          let next = f x
          pure $ next
      notDup seen (St tip _) = not $ member tip seen
      dedup :: [St] -> [St]
      dedup = dedup' Set.empty
      dedup' _ [] = []
      dedup' a (b:c) = if Set.member (getTuple b) a
          then dedup' a c
          else b: dedup' (Set.insert (getTuple b) a) c

data St = St
    { getTuple :: Tuple
    , getTrace :: [String]
    }

instance Show St where
    show (St (a, b, c, d, e, f) log) =
        printf "  %i\n %i %i\n%i %i %i\n%s"
          a b c d e f $ show (reverse log)

mkState tuple = St tuple []

trans f name (St tuple log) = St (f tuple) (name : log)
center = trans center_ "C"
top = trans top_ "T"
left = trans left_ "L"
right = trans right_ "R"

all_trans = [center, top, left, right]

type Tuple =
    ( Int
    , Int, Int
    , Int, Int, Int)

center_ :: Tuple -> Tuple
center_
    ( a
    , b, c
    , d, e, f) =
    ( a
    , e, b
    , d, c, f )
top_ :: Tuple -> Tuple
top_
    ( a
    , b, c
    , d, e, f) =
    ( b
    , c, a
    , d, e, f)
left_ :: Tuple -> Tuple
left_
    ( a
    , b, c
    , d, e, f) =
    ( a
    , d, c
    , e, b, f)
right_ :: Tuple -> Tuple
right_
    ( a
    , b, c
    , d, e, f) =
    ( a
    , b, e
    , d, f, c)
