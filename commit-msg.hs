module Main where

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.Process (readProcess)
import Control.Monad (liftM)
import Data.List (stripPrefix)

main :: IO ()
main = getArgs >>= parse >>= commitMsg >>= putStr

commitMsg :: String -> IO String
commitMsg text = liftM unlines $ updateFirstLine $ lines text

updateFirstLine :: [String] -> IO [String]
updateFirstLine (first:rest) = fmap (: rest) (update first)
updateFirstLine [] = return []

update :: String -> IO String
update base = fmap ((base ++) . suffix) currentBranch

suffix :: String -> String
suffix string =
  if match
    then " - issue #" ++ number
    else ""
  where
    (match, number) =
      case stripPrefix "issue" string of
        Just rest -> (True, rest)
        Nothing -> (False, undefined)

git :: [String] -> String -> IO String
git = readProcess "git" -- missing parameters here

currentBranch :: IO String
currentBranch = git ["rev-parse", "--abbrev-ref", "HEAD"] ""

parse :: [String] -> IO String
parse ["-h"] = usage   >> exitSuccess
parse ["-v"] = version >> exitSuccess
parse []     = exitFailure
parse args   = readFile $ head args

usage :: IO ()
usage = putStrLn "Usage: commit-msg [-vh] file"

version :: IO ()
version = putStrLn "0.0.1"
