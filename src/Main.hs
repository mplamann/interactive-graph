module Main where

import Control.Monad
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import System.Environment

import Graph

main = do
  args <- getArgs
  case args of
    [file] -> do
      startGUI defaultConfig {
        tpPort = Just 9000
        , tpStatic = Just "../wwwroot"
        } (setup file)
    _ -> getProgName >>= \p -> putStrLn ("Usage: " ++ p ++ " file")

doNode (Node _ desc neighbors) text div = do
  element text # set UI.text desc
  set children [] (div)
  forM_ neighbors $ \(str, node) -> do
    button <- UI.button # set UI.text str
    on UI.click button $ const $ do
      doNode node text div
    div #+ [element button]

setup filename window = do
  return window # set UI.title "Graph Explorer"
  text <- UI.p # set UI.text "hi" # set UI.id_ "responseText"
  div <- UI.div
  button <- UI.button # set UI.text "Click me!"
  element div #+ [element button]
  getBody window #+ [element text, element div]
  on UI.click button $ const $ do
    element button # set UI.text "I have been clicked!"
    button2 <- UI.button # set UI.text "Who, me?"
    element div #+ [element button2]
    element text # set UI.text "Button was clicked!"
  file <- liftIO $ readFile filename
  doNode (loadFromString file) text (return div)