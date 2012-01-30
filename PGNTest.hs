module Main where

import Data.ByteString.Char8 (pack, unpack)
import IO
import PGN
import Data.Attoparsec.ByteString.Char8
import Control.Monad.Instances
import Control.Monad
import Chess
import Data.Either

loadTestGame = do
  file <- openFile "testgame.pgn" ReadMode
  pgn <- hGetContents file
  return $ pack pgn

main = do
  pgn <- loadTestGame
  let eithergames = parseOnly pgnParser pgn
  case eithergames of
    Left err -> error err
    Right games -> do 
      mapM_ (uncurry gameprint) $ zip (map (moveSequence defaultBoard . moves) games) games

-- prettyprint a game with result
gameprint (Left err) pgn = do
  putStrLn "========"
  putStrLn $ (event pgn) ++ " " ++ (date pgn) ++ " (" ++ whitePlayer pgn ++ " - " ++ blackPlayer pgn ++ ") " ++ (show $ result pgn)
  putStrLn $ "Error in game: " ++ show err
  putStrLn "--------"
  print pgn
  putStrLn "========"
gameprint (Right board) pgn = do
  putStrLn "========"
  putStrLn $ (event pgn) ++ " " ++ (date pgn) ++ " (" ++ whitePlayer pgn ++ " - " ++ blackPlayer pgn ++ ") " ++ (show $ result pgn)
  putStrLn "--------"
  putStr $ show board
  putStrLn "--------"
  putStrLn $ "Checkmate: " ++ matestr
  putStrLn $ "Stalemate: " ++ stalematestr
  putStrLn "========"
  where matestr
          | mate White board = "white"
          | mate Black board = "black"
          | otherwise = "no"
        stalematestr
          | stalemate (turn board) board = show $ turn board
          | otherwise = "no"

-- apply the moves to the board, giving a detailed error in case of failure
moveSequence brd mvs = foldM (flip moveVerbose) brd mvs

moveVerbose mv brd = case moveSAN mv brd of
  Right b -> Right b
  Left er -> Left (mv, brd, turn brd, castlingAvail brd, er)