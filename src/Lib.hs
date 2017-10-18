module Lib
    ( someFunc
    ) where

import Control.Monad
import Data.Functor.Identity
import Debug.Trace
import Data.Set as Set
import Data.List
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
solution =
    take 1 $ match_filter $ all_combinations
  where
    match_filter = Prelude.filter match_state
    match_state (St tup _) = match tup

all_combinations :: [St]
all_combinations =
    concat $ iterate (\x -> (go . dedup $ x)) [mkState initial]
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
        (runIdentity a)
        (runIdentity b)
        (runIdentity c)
        (runIdentity d)
        (runIdentity e)
        (runIdentity f)
        (show (reverse log))

mkState tuple = St tuple []

trans f name (St tuple log) = St (f tuple) (name : log)
center = trans center_ "C"
top = trans top_ "T"
left = trans left_ "L"
right = trans right_ "R"

all_trans = [center, top, left, right]

type TupleF f =
    ( f Int
    , f Int, f Int
    , f Int, f Int, f Int)

type Tuple = TupleF Identity
type MatchingTuple = TupleF Maybe

liftTuple :: (Int -> f Int) -> (Int, Int, Int, Int, Int, Int) -> TupleF f
liftTuple fn (a, b, c, d, e, f) =
    (fn a, fn b, fn c, fn d, fn e, fn f)

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

is_transformable :: Tuple -> Tuple -> Bool
is_transformable from to =
    edit_distance `mod` 2 == 0
  where
    edit_distance =
        snd $ Prelude.foldl transform_step (from, 0) $ sliding_matchers to
    transform_step state matcher =
        let dist1 = elemental_transforms
            dist2 = [ a . b | a <- elemental_transforms, b <- elemental_transforms]
        in head $ [ next
                  | transform <- dist1 ++ dist2
                  , let next@(tuple, _) = transform state
                  , matcher tuple ]
    sliding_matchers target =
        let match_all matchers = \tuple -> and $ fmap ($ tuple) matchers
        in fmap match_all $ inits (matchers target)
    matchers (a, b, c, d, e, f) =
        [ \(a', _, _, _, _, _) -> a == a'
        , \(_, b', _, _, _, _) -> b == b'
        , \(_, _, c', _, _, _) -> c == c'
        , \(_, _, _, d', _, _) -> d == d'
        , \(_, _, _, _, e', _) -> e == e'
        , \(_, _, _, _, _, f') -> f == f'
        ]
    elemental_transforms =
        [ \((a, b, c, d, e, f), cnt) -> ((b, a, c, d, e, f), cnt+1)
        , \((a, b, c, d, e, f), cnt) -> ((c, b, a, d, e, f), cnt+1)
        , \((a, b, c, d, e, f), cnt) -> ((a, c, b, d, e, f), cnt+1)
        , \((a, b, c, d, e, f), cnt) -> ((a, d, c, b, e, f), cnt+1)
        , \((a, b, c, d, e, f), cnt) -> ((a, e, c, d, b, f), cnt+1)
        , \((a, b, c, d, e, f), cnt) -> ((a, b, e, d, c, f), cnt+1)
        , \((a, b, c, d, e, f), cnt) -> ((a, b, f, d, e, c), cnt+1)
        , \((a, b, c, d, e, f), cnt) -> ((a, b, c, e, d, f), cnt+1)
        , \((a, b, c, d, e, f), cnt) -> ((a, b, c, d, f, e), cnt+1)
        ]
