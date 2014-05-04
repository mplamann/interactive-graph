module Main where

import Control.Monad

import Graph
import GraphTests

doNode (Node label desc neighbors) = do
  putStrLn desc
  unless (null neighbors) $ do
    putStrLn "Options:"
    mapM_ (putStrLn . ("  " ++ ) . fst) neighbors
    putStr "> "
    choice <- getLine
    doNode (snd $ neighbors !! read choice)

main = do
  file <- readFile "testGraph.flw"
  doNode (loadFromString file)

runTest = do
  putStrLn $ test "mkGraph" testMkGraph
  putStrLn $ test "loadFromString" testLoadFromString
