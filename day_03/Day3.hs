module Day3 where

import Data.Functor ((<&>))
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (fromList, toList)

priority :: [(Char, Integer)]
priority = zip ['a' .. 'z'] [1 .. 26] ++ zip ['A' .. 'Z'] [27 .. 52]

half :: [a] -> ([a], [a])
half s = splitAt (length s `div` 2) s

repeatedElems :: (Ord a, Foldable t) => [a] -> t a -> [a]
repeatedElems xs ys = [x | x <- (toList . fromList) xs, x `elem` ys]

findBadge :: (Ord a, Foldable t1, Foldable t2) => [a] -> t1 a -> t2 a -> [a]
findBadge xs ys zs = [x | x <- (toList . fromList) xs, x `elem` ys, x `elem` zs]

part1 :: IO Integer
part1 =
    readFile "input.txt"
        <&> ( sum
                . map (fromJust . (`lookup` priority))
                . concatMap (uncurry repeatedElems . half)
                . lines
            )

part2 :: IO Integer
part2 =
    readFile "input.txt"
        <&> ( sum
                . map (fromJust . (`lookup` priority))
                . concatMap (\[xs, ys, zs] -> findBadge xs ys zs)
                . chunksOf 3
                . lines
            )
