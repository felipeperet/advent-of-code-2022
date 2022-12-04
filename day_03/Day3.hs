module Day3 where

import Data.Functor    ((<&>))
import Data.List.Split (chunksOf)
import Data.Maybe      (fromJust)
import Data.Set        (toList, fromList)

priority :: [(Char, Integer)]
priority = zip ['a' .. 'z'] [1 .. 26] ++ zip ['A' .. 'Z'] [27 .. 52]

half :: [a] -> ([a], [a])
half s = splitAt (length s `div` 2) s

repeatedElems :: (Ord a, Foldable t) => [a] -> t a -> [a]
repeatedElems xs ys = [ x | x <- (toList . fromList) xs, x `elem` ys ]

findBadge :: (Ord a, Foldable t1, Foldable t2) => [a] -> t1 a -> t2 a -> [a]
findBadge xs ys zs =
    [ x | x <- (toList . fromList) xs, x `elem` ys, x `elem` zs ]

part1 :: IO Integer
part1 =
    readFile "input.txt"
        <&> lines
        <&> concatMap (uncurry repeatedElems . half)
        <&> map (fromJust . (`lookup` priority))
        <&> sum

part2 :: IO Integer
part2 =
    readFile "input.txt"
        <&> lines
        <&> chunksOf 3
        <&> concatMap (\[xs, ys, zs] -> findBadge xs ys zs)
        <&> map (fromJust . (`lookup` priority))
        <&> sum
