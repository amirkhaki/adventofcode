module Main where

part1 :: String -> String 
part1 inp = (\(_, y) -> show y) $ foldl f (50, 0) (map parse (words inp))
  where parse x = let n = read (tail x) in
          if head x == 'R' then n else negate n
        f (x, y) r = let v = (x + r + 100) `mod` 100 in
          (v, if v == 0 then y + 1 else y) 
part2 :: String -> String 
part2 inp = (\(_, y) -> show y) $ foldl f (50, 0) (concat $ map parse (words inp))
  where parse x = let n = read (tail x) in
          if head x == 'R' then take n (repeat 1) else take n (repeat (-1))
        f (x, y) r = let v = (x + r + 100) `mod` 100 in
          (v, if v == 0 then y + 1 else y) 

main :: IO ()
main = interact part2
