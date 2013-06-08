module Chess.FEN ( fromFEN
	             , toFEN
	             , defaultFEN
	             , defaultBoard
	             ) where

import Chess
import qualified Data.List as L
import           Data.Char
import           Data.Array
import           Data.Maybe

split [] delim = [""]
split (c:cs) delim
   | c == delim = "" : rest
   | otherwise = (c : head rest) : tail rest
   where rest = split cs delim

unsplit [] delim = ""
unsplit (x:[]) delim = x
unsplit (x:xs) delim = x ++ delim ++ (unsplit xs delim)

fromFEN :: String -> Maybe Board
fromFEN fen = readPosition $ words fen
  where readPosition (pieces:turn:castle:enpassant:_) =
          Just $ Board (clr turn) castle enpas board where
            clr x = if x == "w" then White else Black
            enpas = if enpassant == "-" then Nothing else Just $ strToPos enpassant
            board = listArray ((0,0),(7,7)) (concat $ L.transpose $ map makeLine (reverse boardLines))
            boardLines = split pieces '/'
            makeLine ls = foldr ((++) . pcs) [] ls
            pcs a = if isDigit a then replicate (digitToInt a) Nothing else [Just (read [a])]
        readPosition _ = Nothing

toFEN :: Board -> String
toFEN brd = pieces ++ " " ++ turnstr ++ " " ++ castString ++ " " ++ enpassantstr where
  pieces = unsplit (map fenline $ [ [ (board brd)!(j,7-i) | j<-[0..7]] | i<-[0..7]]) "/"
  turnstr = if turn brd == White then "w" else "b"
  enpassantstr = fromMaybe "-" (enpassant brd >>= \(x,y) -> return [chr (x+97), intToDigit (y+1)])
  castString = if castlingAvail brd == "" then "-" else castlingAvail brd
  fenline pcs = concatMap tos $ foldr com [] pcs where
    tos = either show show
    com a b = case a of
      Just x -> (Right x) : b
      Nothing -> case b of
        ((Left n):xs) -> (Left (n+1)) : xs
        _ -> (Left 1) : b

defaultFEN :: String
defaultFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -"

defaultBoard :: Board
defaultBoard = fromJust $ fromFEN defaultFEN