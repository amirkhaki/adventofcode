module Main where

import Data.Char
import Data.List

split v [] = []
split v (x:xs) = if v == x then xs else split v xs

maxList n [] = error "empty list"
maxList n xs = foldl1 (\b a -> if length(split a xs) >= n then max b a else b) xs

part1 :: String -> String
part1 inp = (show . sum) $ map (f1 . map (digitToInt)) (words inp)
  where f1 xs = let m = maxList 1 xs
                    ds = split m xs in
                  m * 10 + (maxList 0 ds)

part2 :: String -> String
part2 inp = (show . sum) $ map ((f1 12 0) . map (digitToInt)) (words inp)
  where f1 0 o xs = o
        f1 n o xs = let m = maxList (n-1) xs
                        ds = split m xs in
                      f1 (n-1) (o * 10 + m) ds

main :: IO ()
main = interact part2
