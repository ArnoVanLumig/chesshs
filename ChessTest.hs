module Main where

import Test.Hspec
import Test.Hspec.HUnit
import Test.HUnit
import Data.Char
import Data.Either
import Chess
import Control.Monad.Instances
import Control.Monad

assertNothing msg x = assertEqual msg Nothing x
assertJust msg (Just x) = return ()
assertJust msg (Nothing) = assertFailure msg
assertEmpty lst = assertEqual "" [] lst
must_eq actual expected = assertEqual "" expected actual

defaultFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -"
defaultBoard = fromFEN defaultFEN

begpos = describe "on the beginposition" [
  it "should not allow moving a white piece in blacks turn"
    (let brd = allowedMove "b1c3" defaultBoard in
     move "b2b3" brd `must_eq` Left WrongTurn),
  it "should not allow moving a nonexistent piece"
    (move "a3a4" defaultBoard `must_eq` Left NoPiece)
  ]

brutepos = "8/3k1p2/8/8/4NB2/8/1RP5/4K3 w - -"
brutemoves = ["b2b1", "b2b3", "b2b4", "b2b5", "b2b6", "b2b7", "b2b8", "b2a2", "c2c3", "c2c4", "e1d1", "e1d2", "e1e2", "e1f2", "e1f1", "e4d2", "e4c3", "e4c5", "e4d6", "e4f6", "e4g5", "e4g3", "e4f2", "f4e3", "f4d2", "f4c1", "f4e5", "f4d6", "f4c7", "f4b8", "f4g5", "f4h6", "f4g3", "f4h2"]
pieces = ["b2", "c2", "e4", "e1", "f4"]
enumpos = let brd = fromFEN brutepos in
  describe "brute force" [
    it "should accept all moves" (
       assertEmpty (filter (\x -> not $ valid x brd) brutemoves)
       ),
    it "shouldn't accept any other move" (
      assertEmpty (filter (\x -> valid x brd) (mulMovesExcept pieces brutemoves))
      )
    ]

pawnmovepos = "8/8/8/4P3/8/8/8/8 w - -"
pawnCapturePosA = "8/8/8/8/8/2pppp2/2PPPP2/8 w - -"
pawnCaptureAAllowed = ["c2d3", "d2c3", "d2e3", "e2d3", "e2f3", "f2e3"]
piecesA = ["c2", "d2", "e2", "f2"]
pawnCapturePosB = "8/8/8/8/8/2pppp2/2PPPP2/8 b - -"
pawnCaptureBAllowed = ["d3c2", "c3d2", "e3d2", "d3e2", "f3e2", "e3f2"]
piecesB = ["c3", "d3", "e3", "f3"]
enPassantPos = "8/3p4/8/8/4P3/8/8/8 w - -"
pawn = describe "a pawn" [
  it "can move forward" (
     let brd = fromFEN pawnmovepos in
     case move "e5e6" brd of
       Right a -> pieceAtStr "e5" a == Nothing && 
                  pieceAtStr "e6" a == Just (Piece White Pawn)
       Left a -> error (show a)
     ),
  it "can't move anywhere else" (
    let brd = fromFEN pawnmovepos in
    assertEmpty $ filter (\x -> valid x brd) (movesExcept "e5" ["e5e6"])
    ),
  it "can capture pieces 1" (
    let brd = fromFEN pawnCapturePosA in
    assertEmpty $ filter (\x -> not $ valid x brd) pawnCaptureAAllowed
    ),
  it "can't do anything else 1" (
    let brd = fromFEN pawnCapturePosA in
    assertEmpty $ filter (\x -> valid x brd) (mulMovesExcept pieces pawnCaptureAAllowed)
    ),
  it "can capture pieces 2" (
    let brd = fromFEN pawnCapturePosB in
    assertEmpty $ filter (\x -> not $ valid x brd) pawnCaptureBAllowed
    ),
  it "can't do anything else 2" (
    let brd = fromFEN pawnCapturePosB in
    assertEmpty $ filter (\x -> valid x brd) (mulMovesExcept pieces pawnCaptureBAllowed)                                
    ),
  it "can do an enpassant capture" (
    let brd = allowedMoves ["e4e5", "d7d5", "e5d6"] (fromFEN enPassantPos) in
    pieceAt 3 4 brd == Nothing &&
    pieceAt 3 5 brd /= Nothing
    )
  ]

rookmovepos = "8/8/8/4R3/8/8/8/8 w - -"
rookPos = "e5"
rookAllowed = ["e5e1", "e5e2", "e5e3", "e5e4", "e5e6", "e5e7", "e5e8", "e5a5", "e5b5", "e5c5", "e5d5", "e5f5", "e5g5", "e5h5"]

rook = let brd = fromFEN rookmovepos in
  describe "a rook" [
    it "can move straight" (
       and $ map (\x -> valid x brd) rookAllowed
       ),
    it "can't move anywhere else" (
      and $ map (\x -> not $ valid x brd) (movesExcept rookPos rookAllowed)
      )
    ]

knightpos = "8/8/8/3n4/8/8/8/8 b - -"
knightallowed = ["d5c3", "d5b4", "d5b6", "d5c7", "d5e7", "d5f6", "d5f4", "d5e3"]
knight = let brd = fromFEN knightpos in 
  describe "a knight" [
  it "can move like a knight" (
     and $ map (\x -> valid x brd) knightallowed
     ),
  it "can't move anywhere else" (
    and $ map (\x -> not $ valid x brd) (movesExcept "d5" knightallowed)
    )
  ]

bishoppos = "8/8/8/3b4/8/8/8/8 b - -"
bishopallowed = ["d5a2", "d5b3", "d5c4", "d5e6", "d5f7", "d5g8", "d5h1", "d5g2", "d5f3", "d5e4", "d5c6", "d5b7", "d5a8"]
bishop = let brd = fromFEN bishoppos in
  describe "a bishop" [
  it "can move diagonally" (
     and $ map (\x -> valid x brd) bishopallowed
     ),
  it "can't move anywhere else" (
    and $ map (\x -> not $ valid x brd) (movesExcept "d5" bishopallowed)
    )
  ]

queenpos = "8/8/8/3q4/8/8/8/8 b - -"
queenallowed = ["d5a2", "d5b3", "d5c4", "d5e6", "d5f7", "d5g8", "d5h1", "d5g2", "d5f3", "d5e4", "d5c6", "d5b7", "d5a8", "d5d1", "d5d2", "d5d3", "d5d4", "d5d6", "d5d7", "d5d8", "d5a5", "d5b5", "d5c5", "d5e5", "d5f5", "d5g5", "d5h5"]
queen = let brd = fromFEN queenpos in
  describe "a queen" [
  it "can move" (
     and $ map (\x -> valid x brd) queenallowed
     ),
  it "can't move anywhere else" (
    and $ map (\x -> not $ valid x brd) (movesExcept "d5" queenallowed)
    )
  ]

kingpos = "8/8/8/3k4/8/8/8/8 b - - 0 1"
kingallowed = ["d5d4", "d5c4", "d5c5", "d5c6", "d5d6", "d5e6", "d5e5", "d5e4"]
king = let brd = fromFEN kingpos in
  describe "a king" [
    it "can move exactly one square" (
       and $ map (\x -> valid x brd) kingallowed
       ),
    it "can't move anywhere else" (
      and $ map (\x -> not $ valid x brd) (movesExcept "d5" kingallowed)
      )
    ]

kingcastlepos = "8/8/8/8/8/8/8/4K2R w KQkq - "
kingCastleInbetweenPos = "8/8/8/8/8/8/8/4K1NR w KQkq -"
kingCastleCheckPosA = "8/8/8/5r2/8/8/8/4K2R w KQkq -"
kingCastleCheckPosB = "8/8/8/6r1/8/8/8/4K2R w KQkq -"
kingcastletest = describe "a kingside castle" [
  it "must be accepted" (
     let brd = fromFEN kingcastlepos in
     case move "0-0" brd of
       Right brd -> do
                    pieceAt 4 0 brd `must_eq` Nothing
                    pieceAt 7 0 brd `must_eq` Nothing
                    pieceAt 5 0 brd `must_eq` Just (Piece White Rook)
                    pieceAt 6 0 brd `must_eq` Just (Piece White King)
       Left err -> return ()
     ),
  it "must not be allowed when piece inbetween" (
    let brd = fromFEN kingCastleInbetweenPos in
    not $ valid "0-0" brd
    ),
  it "must not be allowed when causes check" (
    let brd = fromFEN kingCastleCheckPosA in
    not $ valid "0-0" brd
    ),
  it "must not be allowed when check inbetween" (
    let brd = fromFEN kingCastleCheckPosA in
    not $ valid "0-0" brd
    )
  ]

queenCastlePos = "8/8/8/8/8/8/8/R3K3 w KQkq -"
queenCastleInbetweenPos = "8/8/8/8/8/8/8/R2BK3 w KQkq -"
queenCastleCheckPosA = "8/8/8/8/6b1/8/8/R3K3 w KQkq -"
queenCastleCheckPosB = "8/8/8/8/5b2/8/8/R3K3 w KQkq -"
queencastletest = describe "a queenside castle" [
  it "must be accepted" (
     let brd = fromFEN queenCastlePos :: Board in
     case move "0-0-0" brd of
       Right brd -> pieceAt 4 0 brd == Nothing &&
                    pieceAt 7 0 brd == Nothing &&
                    pieceAt 2 0 brd == Just (Piece White Rook) &&
                    pieceAt 1 0 brd == Just (Piece White King)
       Left err -> False
     ),
  it "must not be allowed when piece inbetween" (
    let brd = fromFEN queenCastleInbetweenPos in
    not $ valid "0-0-0" brd
    ),
  it "must not be allowed when causes check" (
    let brd = fromFEN queenCastleCheckPosA in
    not $ valid "0-0-0" brd
    ),
  it "must not be allowed when check inbetween" (
    let brd = fromFEN queenCastleCheckPosA in
    not $ valid "0-0-0" brd
    )
  ]

checkMoveA = "8/4r3/8/8/3p4/4P3/4K3/8 w - -"
checkMoveB = "8/8/8/8/8/8/3KR2r/8 w - -"
checkmoves = describe "moves causing check" [
  it "must not be allowed A" (
     let brd = fromFEN checkMoveA in
     move "e3d4" brd == Left CausesCheck
     ),
  it "must not be allowed B" (
     let brd = fromFEN checkMoveB in
     move "e2e3" brd == Left CausesCheck
     )
  ]

kingCastleCheck = "8/8/8/3r4/3B4/8/8/R3K2R b KQkq -"
kingCastle = "8/p7/8/8/8/8/8/4K2R w KQkq -"
queenCastle = "8/p7/8/8/8/8/8/R3K3 w KQkq -"
castletest = describe "castling" [
  it "must not be allowed kingside when king was check" (do
     let brd = allowedMoves ["d5e5", "d4e3", "e5d5"] $ fromFEN kingCastleCheck
     move "0-0" brd `must_eq` Left InvalidMove
     move "0-0-0" brd `must_eq` Left InvalidMove
     ),
  it "must not be allowed kingside when kingrook moved" (do
    let brd = allowedMoves ["h1h2", "a7a6", "h2h1", "a6a5"] $ fromFEN kingCastle
    move "0-0" brd `must_eq` Left InvalidMove
    ),
  it "must not be allowed queenside when queenrook has moved" (do
    let brd = allowedMoves ["a1a2", "a7a6", "a2a1", "a6a5"] $ fromFEN queenCastle
    move "0-0-0" brd `must_eq` Left InvalidMove
    ),
  it "must not be allowed when king has moved" (do
    let brd = allowedMoves ["e1e2", "a7a6", "e2e1"] $ fromFEN kingCastle
    move "0-0" brd `must_eq` Left InvalidMove
    )
  ]

pawnPromotionPos = "8/P7/8/8/8/8/8/8 w KQkq -"
pawnPromotionCheck = "8/2K3Pr/8/8/8/8/8/8 w KQkq -"
promotion = describe "promotion" [
  it "must be allowed when pawn on last row" (
     let brd = fromFEN pawnPromotionPos in
     case move "a7a8q" brd of
       Right b -> pieceAt 0 7 b == Just (Piece White Queen) &&
                  pieceAt 0 6 b == Nothing
       _ -> False
     ),
  it "must not be allowed when it causes check" (
    let brd = fromFEN pawnPromotionCheck in
    move "g7g8q" brd == Left CausesCheck
    )
  ]

fentest = describe "fen" [
  it "must be equal to input fen" (do
     toFEN (fromFEN pawnPromotionPos) `must_eq` pawnPromotionPos
     toFEN (fromFEN pawnPromotionCheck) `must_eq` pawnPromotionCheck
     toFEN (fromFEN checkMoveA) `must_eq` checkMoveA
     toFEN (fromFEN checkMoveB) `must_eq` checkMoveB
     toFEN (fromFEN kingCastle) `must_eq` kingCastle
     toFEN (fromFEN kingCastleCheck) `must_eq` kingCastleCheck
     toFEN (fromFEN queenCastle) `must_eq` queenCastle
     toFEN (fromFEN staleMatePos) `must_eq` staleMatePos
     toFEN (fromFEN queenCastleCheckPosA) `must_eq` queenCastleCheckPosA
     toFEN (fromFEN queenCastleCheckPosB) `must_eq` queenCastleCheckPosB
     )
  ]

staleMatePos = "7k/8/6r1/4K3/6r1/3r1r2/8/8 w - -"
stalematetest = describe "stalemate" [
  it "should be detected" (
     stalemate White $ fromFEN staleMatePos
     )
  ]

matePos = "3K2r1/6r1/8/8/8/8/8/2k5 w - -"
matetest = describe "mate" [
  it "should be detected" (
     mate White $ fromFEN matePos
     )
  ]

mulMovesExcept pcs lst = concat $ map (\x -> movesExcept x lst) pcs
movesExcept pc lst = (filter (not . flip elem lst) (allMoves pc))
allMoves pc = [pc ++ [(chr (x+97)), (intToDigit (y+1))] | x<-[0..7], y<-[0..7]]

tests = descriptions [begpos, enumpos, pawn, rook, knight, bishop, queen, king, kingcastletest, queencastletest, checkmoves, castletest, promotion, fentest, stalematetest, matetest]

main = do
  hspec tests

valid a brd = case move a brd of
  Right brd -> True
  _ -> False

allowedMove a brd = case move a brd of
    Right brd -> brd
    Left f -> error ("Move " ++ (show a) ++ " not allowed: " ++ (show f))

allowedMoves mvs brd = foldl (\x y -> allowedMove y x) brd mvs

moveSequence brd mvs = foldM (flip moveVerbose) brd mvs

moveVerbose mv brd = case move mv brd of
  Right b -> Right b
  Left er -> Left (mv, er)