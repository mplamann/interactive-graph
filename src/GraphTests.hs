{-# LANGUAGE OverloadedStrings #-}

module GraphTests where

import Data.Attoparsec.Char8
import Data.ByteString.Char8 (pack)
import Graph

test :: String -> Bool -> String
test fname val = "Test " ++ fname ++ ": " ++ assert val

assert :: Bool -> String
assert True = "Passed"
assert False = "Failed"

testMkGraph = let graph =
                   mkGraph [("root", "This is the root node", [("Go to the root node",
                                                                "root")])]
                  root = graph
                  [("Go to the root node",link)] = neighbors root
             in (label root) == "root" &&
                (text root) == "This is the root node" &&
                (label link) == "root" &&
                (text link) == "This is the root node"

testLoadFromString = 
  let graph = loadFromString
              "[root]\nThis is the root node\n[[root]Go to the root node]"
      root = graph
      [("Go to the root node",link)] = neighbors root
  in (label root) == "root" &&
     (text root) == "This is the root node" &&
     (label link) == "root" &&
     (text link) == "This is the root node"

testParse =
  let Right result = parseOnly nodeLabel "[root]"
  in result == "root"