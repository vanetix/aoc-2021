module Main where

import Data.List (tails)
import Data.Maybe (mapMaybe)
import GHC.Num (Num)
import System.Environment (getArgs)

data Command
  = Forward Integer
  | Up Integer
  | Down Integer
  deriving (Read, Show)

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile $ head args

  let input = mapMaybe parseCommand $ lines contents

  putStrLn $ "Part 1 solution: " ++ show (partOne input)
  putStrLn $ "Part 2 solution: " ++ show (partTwo input)

  return ()

parseCommand :: String -> Maybe Command
parseCommand s = case words s of
  [dir, num] ->
    case dir of
      "forward" -> Just $ Forward (read num :: Integer)
      "up" -> Just $ Up (read num :: Integer)
      "down" -> Just $ Down (read num :: Integer)
      _ -> Nothing
  _ -> Nothing

partOne :: [Command] -> Integer
partOne commands =
  let (position, depth) = foldl f (0, 0) commands
   in position * depth
  where
    f (position, depth) (Forward c) = (position + c, depth)
    f (position, depth) (Up c) = (position, depth - c)
    f (position, depth) (Down c) = (position, depth + c)

partTwo :: [Command] -> Integer
partTwo commands =
  let (position, depth, _) = foldl f (0, 0, 0) commands
   in position * depth
  where
    f (position, depth, aim) (Forward c) = (position + c, depth + (aim * c), aim)
    f (position, depth, aim) (Up c) = (position, depth, aim - c)
    f (position, depth, aim) (Down c) = (position, depth, aim + c)
