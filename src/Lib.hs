module Lib
    ( someFunc
    ) where

import Control.Monad
import Data.Functor.Identity
import Debug.Trace
import Data.Set as Set
import Data.List
import Text.Printf
import Text.Read

someFunc :: IO ()
someFunc = do
    initial <- getInitial
    patt <- getPatt
    if is_transformable patt initial
        then putStrLn . showTrace $ solution patt initial
        else putStrLn "insolvable"
    someFunc

getInput :: String -> (String -> Maybe a) -> IO a
getInput header parse = do
    putStrLn header
    line <- getLine
    case parse line of
      Just res -> return res
      Nothing -> do
          putStr "(retry) "
          getInput header parse

getInitial =
    getInput "initial placement:" $
    \line -> do
        let w = words line
        guard $ length w == 6
        l@[a, b, c, d, e, f] <- mapM readMaybe w
        guard $ all (\x -> x >= 1 && x <= 6) l
        return $ liftTuple Identity (a, b, c, d, e, f)

getPatt =
    getInput "target pattern (0 to blank): " $
    \line -> do
        let w = words line
        guard $ length w == 6
        l@[a, b, c, d, e, f] <- mapM readMaybe w
        guard $ all (\x -> x >= 1 && x <= 6) l
        return $ liftTuple zeroToNothing (a, b, c, d, e, f)
  where
      zeroToNothing x = if x == 0 then Nothing else Just x

solution :: PatternTuple -> Tuple -> St
solution patt initial =
    head $ Prelude.filter (matches patt . getTuple) $ all_combinations
  where
    all_combinations =
        concat $ iterate step [mkState initial]
    step xs =
        [ f x
        | x <- xs
        , f <- all_trans ]

data St = St
    { getTuple :: Tuple
    , getTrace :: [String]
    }

showTrace St { getTrace = log } =
    concat $ intersperse " -> " (reverse log)

instance Show St where
    show st@(St tuple log) =
        let Just (a, b, c, d, e, f) = unliftTuple (Just . runIdentity) tuple
        in printf "  %i\n %i %i\n%i %i %i\n%s" a b c d e f $ showTrace st

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
type PatternTuple = TupleF Maybe

liftTuple :: (Int -> f Int) -> (Int, Int, Int, Int, Int, Int) -> TupleF f
liftTuple fn (a, b, c, d, e, f) =
    (fn a, fn b, fn c, fn d, fn e, fn f)

unliftTuple :: (f Int -> Maybe Int) -> TupleF f -> Maybe (Int, Int, Int, Int, Int, Int)
unliftTuple fn (a, b, c, d, e, f) = do
    [a', b', c', d', e', f'] <- mapM fn [a, b, c, d, e, f]
    pure (a', b', c', d', e', f')

matches :: PatternTuple -> Tuple -> Bool
matches (a', b', c', d', e', f') (a, b, c, d, e, f) =
    let pairs = [(a, a'), (b, b'), (c, c'), (d, d'), (e, e'), (f, f')]
    in all (\(x, x') -> maybe True (== runIdentity x) x') pairs

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

is_transformable :: PatternTuple -> Tuple -> Bool
is_transformable patt input =
    edit_distance `mod` 2 == 0
  where
    edit_distance =
        snd $ Prelude.foldl transform_step (input, 0) $ sliding_matchers patt
    transform_step state matcher =
        let dist1 = elemental_transforms
            dist2 = [ a . b | a <- elemental_transforms, b <- elemental_transforms]
        in head $ [ next
                  | transform <- dist1 ++ dist2
                  , let next@(tuple, _) = transform state
                  , matcher tuple ]
    sliding_matchers :: PatternTuple -> [(Tuple -> Bool)]
    sliding_matchers patt =
        let compose matchers = \tuple -> and $ fmap ($ tuple) matchers
        in fmap compose $ inits (matchers patt)
    matchers :: PatternTuple -> [(Tuple -> Bool)]
    matchers (a, b, c, d, e, f) =
        let matchWith x y = maybe True (== runIdentity y) x
        in
          [ \(a', _, _, _, _, _) -> a `matchWith` a'
          , \(_, b', _, _, _, _) -> b `matchWith` b'
          , \(_, _, c', _, _, _) -> c `matchWith` c'
          , \(_, _, _, d', _, _) -> d `matchWith` d'
          , \(_, _, _, _, e', _) -> e `matchWith` e'
          , \(_, _, _, _, _, f') -> f `matchWith` f'
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
