module Main where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (liftM)
import Data.List (stripPrefix)
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.Process (readProcess)

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse ["-h"] = usage   >> exitSuccess
parse ["-v"] = version >> exitSuccess
parse []     = exitFailure
parse args   = readFile file >>= evaluate . force >>= commitMsg >>= writeFile file
  where file = head args

commitMsg :: String -> IO String
commitMsg text = liftM unlines $ updateFirstLine $ lines text

updateFirstLine :: [String] -> IO [String]
updateFirstLine (first:rest) = fmap (: rest) (update first)
updateFirstLine [] = return []

update :: String -> IO String
update base = fmap ((base ++) . suffix) currentBranch

suffix :: String -> String
suffix string = maybe "" (" - issue #"++) (stripPrefix "issue" string)

git :: [String] -> String -> IO String
git = readProcess "git" -- missing parameters here

currentBranch :: IO String
currentBranch = git ["rev-parse", "--abbrev-ref", "HEAD"] ""

usage :: IO ()
usage = putStrLn "Usage: commit-msg [-vh] file"

version :: IO ()
version = putStrLn "0.0.1"
