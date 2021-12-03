module Main where

import Data.List (tails)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile $ head args

  let input = (\x -> read x :: Integer) <$> lines contents

  print $ "Part 1 answer: " ++ show (partOne input)
  print $ "Part 2 answer: " ++ show (partOne . map (\(a, b, c) -> a + b + c) $ window 3 input)

  return ()

window :: Int -> [a] -> [(a, a, a)]
window n l = map (\[a, b, c] -> (a, b, c)) . filter f . map (take n) $ tails l
  where
    f [_, _, _] = True
    f _ = False

partOne :: [Integer] -> Integer
partOne = snd . foldl f (Nothing, 0)
  where
    f (Nothing, acc) item = (Just item, acc)
    f (Just prev, acc) item = (Just item, if prev < item then acc + 1 else acc)
