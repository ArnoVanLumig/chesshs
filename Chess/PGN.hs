{-# LANGUAGE OverloadedStrings #-}
module Chess.PGN ( pgnParser
                 , PGN(..)
                 , GameResult(..)) where

import Chess
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (pack, unpack)
import Data.Map (fromList, (!))

type Move = String

data PGN = PGN { event :: String
               , site :: String
               , date :: String
               , round :: String
               , whitePlayer :: String
               , blackPlayer :: String
               , result :: Maybe GameResult
               , initialPosition :: Maybe Board
               , moves :: [Move]
               } deriving (Show)

data GameResult = WhiteWon
                | BlackWon
                | Draw
                deriving (Eq, Show)

pgnParser = many gameParse

gameParse = do
  skipSpace
  tagsTups <- many1 parseTag
  let tags = fromList tagsTups
  let gameResult = case tags ! "Result" of
        "1/2-1/2" -> Just Draw
        "1-0"     -> Just WhiteWon
        "0-1"     -> Just BlackWon
        _         -> Nothing
  moves <- many parseMove
  many uselessStuff
  endResult
  return $ PGN (unpack $ tags ! "Event")
               (unpack $ tags ! "Site")
               (unpack $ tags ! "Date")
               (unpack $ tags ! "Round")
               (unpack $ tags ! "White")
               (unpack $ tags ! "Black")
               gameResult
               Nothing
               moves

-- todo: handle escaping
stringLiteral = do
  char '"'
  value <- takeTill ((==) '"')
  char '"'
  return value

parseTag = do
  skipSpace
  char '['
  tagType <- takeTill ((==) ' ')
  skipSpace
  tagValue <- stringLiteral
  char ']'
  return (tagType, tagValue)

moveNumber = do
  decimal
  many $ char '.'
  whitespace

nag = do
  char '$'
  decimal
  whitespace

rav = do
  char '('
  scan 1 (\s a -> let news = if a == '('
                             then s+1
                             else (if a == ')'
                                   then s-1
                                   else s) in
                  if news == 0 then Nothing else Just news)
  char ')'

comment = braceCmt <|> semiCmt where
  braceCmt = do
    char '{'
    cmt <- takeTill ((==) '}')
    char '}'
    return cmt
  semiCmt = do
    char ';'
    cmt <- takeTill ((==) '\n')
    char '\n'
    return cmt

discard a = do
  a
  return ()

whitespace = discard (char ' ') <|>
             discard (char '\n') <|>
             discard (string "\r\n") <|>
             discard (char '\t')

uselessStuff = discard moveNumber <|>
               discard comment <|>
               discard whitespace <|>
               discard nag <|>
               discard rav

endResult = string "1-0" <|>
            string "0-1" <|>
            string "1/2-1/2" <|>
            string "*"

parseMove = do
  skipMany uselessStuff
  movestr <- many1 $ satisfy (not . isSpace)
  if movestr `elem` ["1-0", "0-1", "1/2-1/2", "*"] then
    fail "end of game reached"
    else
    return movestr
