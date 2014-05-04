module Graph where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad
import Data.Attoparsec.Char8
import Data.ByteString.Char8 (pack)
import Data.Maybe (fromJust)
import Debug.Trace

data Node = Node { label :: String
                 , text :: String
                 , neighbors :: [(String,Node)]
                 }

mkGraph :: [(String, String, [(String,String)])] -> Node
mkGraph links = lookupNode "root"
  where
    mkNode (lbl, txt, adj) = (lbl, Node lbl txt $ map (fst &&& lookupNode . snd) adj)
    nodeLookupList = map mkNode links
    lookupNode lbl = fromJust $ lookup lbl nodeLookupList

nodeLabel :: Parser String
nodeLabel = do
  char '['
  label <- many1 (notChar ']')
  char ']'
  many endOfLine
  return label

nodeText :: Parser String
nodeText = do
  text <- (many . satisfy $ \c ->
            c /= '\n' &&
            c /= '\r' &&
            c /= '[' 
          ) `sepBy` endOfLine
  many endOfLine
  return (join text)

nodeLink :: Parser (String, String)
nodeLink = do
  char '['
  skipSpace
  char '['
  target <- many1 (notChar ']')
  char ']'
  desc <- many1 (notChar ']')
  char ']'
  many endOfLine
  return (desc, target)

nodeSpec :: Parser (String, String, [(String, String)])
nodeSpec = (,,) <$> nodeLabel <*> nodeText <*> many nodeLink

loadFromString :: String -> Node
loadFromString str = case parseOnly (many nodeSpec) (pack str) of
  Left _ -> error "Improper string"
  Right l -> mkGraph l
  