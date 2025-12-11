module Main where

import Data.List

split :: (Char -> Bool) -> String -> [String]
split p s = case dropWhile p s of
  "" -> []
  s' -> w : split p s''
    where (w, s'') = break p s'

(<<) :: (Integral a) => a -> a -> a
a << b = a * (10 ^ b)

part1 :: String -> String
part1 inp =  (show . sum) $ map (sum . f2 . f1 . split (=='-')) (split (==',') inp)
  where f1 (x:y:[]) = ((read x, read y), map (`div` 2) $
                        filter even [length x .. length y])
        f2 ((a, b), xs) = concat $  map (f3 a b) xs
        f3 a b i = filter (\x -> (x >= a) && (x <= b)) $
          map (\x -> x << i + x) [10 ^ (i-1) .. 10 ^ i - 1]

divisors 1  = [1]
divisors 2  = [2, 1]
divisors 3  = [3, 1]
divisors 4  = [4, 1, 2]
divisors 5  = [5, 1]
divisors 6  = [6, 1, 2, 3]
divisors 7  = [7, 1]
divisors 8  = [8, 1, 2, 4]
divisors 9  = [9, 1, 3]
divisors 10 = [10, 1, 2, 5]
divisors 11 = [11, 1]
divisors 12 = [12, 1, 2, 3, 4, 6]

repeatD :: Integer -> Integer -> Integer -> Integer -> Integer
repeatD x i o 0 = o
repeatD x i o n = repeatD x i (o << i + x) (n-1)

part2 :: String -> String
part2 inp =  (show . sum . nub . concat) $ map (f2 . f1 . split (=='-')) (split (==',') inp)
  where f1 (x:y:[]) = ((read x :: Integer, read y :: Integer), map divisors
                        [length x .. length y])
        f2 ((a, b), xs) = concat $ map (f3 a b) xs
        f3 a b (d:xs) = concat $ map (f4 a b d) xs
        f4 a b d i = filter (\x -> (x >= a) && (x <= b)) $
          map (\x -> repeatD x i 0 (d `div` i)) [10 ^ (i-1) .. 10 ^ i - 1]


main :: IO ()
main = interact part2
