module Main where

import Test.Hspec
import Test.Hspec.HUnit
import Test.HUnit
import Data.Char
import Data.Either
import Data.Maybe
import Chess
import Chess.FEN
import Control.Monad.Instances
import Control.Monad

assertNothing msg x = assertEqual msg Nothing x
assertJust msg (Just x) = return ()
assertJust msg (Nothing) = assertFailure msg
assertEmpty lst = assertEqual "" [] lst
must_eq actual expected = assertEqual "" expected actual

defaultFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -"

begpos = describe "on the beginposition" $ do
  it "should not allow moving a white piece in blacks turn" $ do
    (let brd = allowedMove "b1c3" defaultBoard in
     move "b2b3" brd `must_eq` Left WrongTurn)
  it "should not allow moving a nonexistent piece" $ do
    move "a3a4" defaultBoard `must_eq` Left NoPiece


brutepos = "8/3k1p2/8/8/4NB2/8/1RP5/4K3 w - -"
brutemoves = ["b2b1", "b2b3", "b2b4", "b2b5", "b2b6", "b2b7", "b2b8", "b2a2", "c2c3", "c2c4", "e1d1", "e1d2", "e1e2", "e1f2", "e1f1", "e4d2", "e4c3", "e4c5", "e4d6", "e4f6", "e4g5", "e4g3", "e4f2", "f4e3", "f4d2", "f4c1", "f4e5", "f4d6", "f4c7", "f4b8", "f4g5", "f4h6", "f4g3", "f4h2"]
pieces = ["b2", "c2", "e4", "e1", "f4"]
enumpos = let Just brd = fromFEN brutepos in
  describe "brute force" $ do
    it "should accept all moves" $ do
      assertEmpty (filter (\x -> not $ valid x brd) brutemoves)
    it "shouldn't accept any other move" $ do
      assertEmpty (filter (\x -> valid x brd) (mulMovesExcept pieces brutemoves))

pawnmovepos = "8/8/8/4P3/8/8/8/8 w - -"
pawnCapturePosA = "8/8/8/8/8/2pppp2/2PPPP2/8 w - -"
pawnCaptureAAllowed = ["c2d3", "d2c3", "d2e3", "e2d3", "e2f3", "f2e3"]
piecesA = ["c2", "d2", "e2", "f2"]
pawnCapturePosB = "8/8/8/8/8/2pppp2/2PPPP2/8 b - -"
pawnCaptureBAllowed = ["d3c2", "c3d2", "e3d2", "d3e2", "f3e2", "e3f2"]
piecesB = ["c3", "d3", "e3", "f3"]
enPassantPos = "8/3p4/8/8/4P3/8/8/8 w - -"
pawn = describe "a pawn" $ do
  it "can move forward" $ do
    let Just brd = fromFEN pawnmovepos in
     case move "e5e6" brd of
       Right a -> pieceAtStr "e5" a == Nothing &&
                  pieceAtStr "e6" a == Just (Piece White Pawn)
       Left a -> error (show a)
  it "can't move anywhere else" $ do
    let Just brd = fromFEN pawnmovepos in
      assertEmpty $ filter (\x -> valid x brd) (movesExcept "e5" ["e5e6"])
  it "can capture pieces 1" $ do
    let Just brd = fromFEN pawnCapturePosA in
      assertEmpty $ filter (\x -> not $ valid x brd) pawnCaptureAAllowed
  it "can't do anything else 1" $ do
    let Just brd = fromFEN pawnCapturePosA in
      assertEmpty $ filter (\x -> valid x brd) (mulMovesExcept pieces pawnCaptureAAllowed)
  it "can capture pieces 2" $ do
    let Just brd = fromFEN pawnCapturePosB in
      assertEmpty $ filter (\x -> not $ valid x brd) pawnCaptureBAllowed
  it "can't do anything else 2" $ do
    let Just brd = fromFEN pawnCapturePosB in
      assertEmpty $ filter (\x -> valid x brd) (mulMovesExcept pieces pawnCaptureBAllowed)
  it "can do an enpassant capture" $ do
    let brd = allowedMoves ["e4e5", "d7d5", "e5d6"] (unsafeFromFEN enPassantPos) in
      pieceAt 3 4 brd == Nothing &&
      pieceAt 3 5 brd /= Nothing

rookmovepos = "8/8/8/4R3/8/8/8/8 w - -"
rookPos = "e5"
rookAllowed = ["e5e1", "e5e2", "e5e3", "e5e4", "e5e6", "e5e7", "e5e8", "e5a5", "e5b5", "e5c5", "e5d5", "e5f5", "e5g5", "e5h5"]

rook = let Just brd = fromFEN rookmovepos in
  describe "a rook" $ do
    it "can move straight" $ do
       and $ map (\x -> valid x brd) rookAllowed
    it "can't move anywhere else" $ do
      and $ map (\x -> not $ valid x brd) (movesExcept rookPos rookAllowed)

knightpos = "8/8/8/3n4/8/8/8/8 b - -"
knightallowed = ["d5c3", "d5b4", "d5b6", "d5c7", "d5e7", "d5f6", "d5f4", "d5e3"]
knight = let Just brd = fromFEN knightpos in
  describe "a knight" $ do
  it "can move like a knight" $ do
     and $ map (\x -> valid x brd) knightallowed
  it "can't move anywhere else" $ do
    and $ map (\x -> not $ valid x brd) (movesExcept "d5" knightallowed)

bishoppos = "8/8/8/3b4/8/8/8/8 b - -"
bishopallowed = ["d5a2", "d5b3", "d5c4", "d5e6", "d5f7", "d5g8", "d5h1", "d5g2", "d5f3", "d5e4", "d5c6", "d5b7", "d5a8"]
bishop = let Just brd = fromFEN bishoppos in
  describe "a bishop" $ do
  it "can move diagonally" $ do
     and $ map (\x -> valid x brd) bishopallowed
  it "can't move anywhere else" $ do
    and $ map (\x -> not $ valid x brd) (movesExcept "d5" bishopallowed)

queenpos = "8/8/8/3q4/8/8/8/8 b - -"
queenallowed = ["d5a2", "d5b3", "d5c4", "d5e6", "d5f7", "d5g8", "d5h1", "d5g2", "d5f3", "d5e4", "d5c6", "d5b7", "d5a8", "d5d1", "d5d2", "d5d3", "d5d4", "d5d6", "d5d7", "d5d8", "d5a5", "d5b5", "d5c5", "d5e5", "d5f5", "d5g5", "d5h5"]
queen = let Just brd = fromFEN queenpos in
  describe "a queen" $ do
  it "can move" $ do
     and $ map (\x -> valid x brd) queenallowed
  it "can't move anywhere else" $ do
    and $ map (\x -> not $ valid x brd) (movesExcept "d5" queenallowed)

kingpos = "8/8/8/3k4/8/8/8/8 b - - 0 1"
kingallowed = ["d5d4", "d5c4", "d5c5", "d5c6", "d5d6", "d5e6", "d5e5", "d5e4"]
king = let Just brd = fromFEN kingpos in
  describe "a king" $ do
    it "can move exactly one square" $ do
       and $ map (\x -> valid x brd) kingallowed
    it "can't move anywhere else" $ do
      and $ map (\x -> not $ valid x brd) (movesExcept "d5" kingallowed)

kingcastlepos = "8/8/8/8/8/8/8/4K2R w KQkq - "
kingCastleInbetweenPos = "8/8/8/8/8/8/8/4K1NR w KQkq -"
kingCastleCheckPosA = "8/8/8/5r2/8/8/8/4K2R w KQkq -"
kingCastleCheckPosB = "8/8/8/6r1/8/8/8/4K2R w KQkq -"
kingcastletest = describe "a kingside castle" $ do
  it "must be accepted" $ do
    let Just brd = fromFEN kingcastlepos in
      case move "O-O" brd of
        Right brd -> do
                     pieceAt 4 0 brd `must_eq` Nothing
                     pieceAt 7 0 brd `must_eq` Nothing
                     pieceAt 5 0 brd `must_eq` Just (Piece White Rook)
                     pieceAt 6 0 brd `must_eq` Just (Piece White King)
        Left err -> return ()
  it "must not be allowed when piece inbetween" $ do
    let Just brd = fromFEN kingCastleInbetweenPos in
      not $ valid "O-O" brd
  it "must not be allowed when causes check" $ do
    let Just brd = fromFEN kingCastleCheckPosA in
      not $ valid "O-O" brd
  it "must not be allowed when check inbetween" $ do
    let Just brd = fromFEN kingCastleCheckPosA in
      not $ valid "O-O" brd

queenCastlePos = "8/8/8/8/8/8/8/R3K3 w KQkq -"
queenCastleInbetweenPos = "8/8/8/8/8/8/8/R2BK3 w KQkq -"
queenCastleCheckPosA = "8/8/8/8/6b1/8/8/R3K3 w KQkq -"
queenCastleCheckPosB = "8/8/8/8/5b2/8/8/R3K3 w KQkq -"
queencastletest = describe "a queenside castle" $ do
  it "must be accepted" $ do
    let Just brd = fromFEN queenCastlePos in
      case move "O-O-O" brd of
        Right brd -> pieceAt 4 0 brd == Nothing &&
                     pieceAt 7 0 brd == Nothing &&
                     pieceAt 3 0 brd == Just (Piece White Rook) &&
                     pieceAt 2 0 brd == Just (Piece White King)
        Left err -> error $ show err
  it "must not be allowed when piece inbetween" $ do
    let Just brd = fromFEN queenCastleInbetweenPos in
      not $ valid "O-O-O" brd
  it "must not be allowed when causes check" $ do
    let Just brd = fromFEN queenCastleCheckPosA in
      not $ valid "O-O-O" brd
  it "must not be allowed when check inbetween" $ do
    let Just brd = fromFEN queenCastleCheckPosA in
      not $ valid "O-O-O" brd

checkMoveA = "8/4r3/8/8/3p4/4P3/4K3/8 w - -"
checkMoveB = "8/8/8/8/8/8/3KR2r/8 w - -"
checkmoves = describe "moves causing check" $ do
  it "must not be allowed A" $ do
    let Just brd = fromFEN checkMoveA in
       move "e3d4" brd == Left CausesCheck
  it "must not be allowed B" $ do
    let Just brd = fromFEN checkMoveB in
       move "e2e3" brd == Left CausesCheck

kingCastle = "8/p7/8/8/8/8/8/4K2R w KQkq -"
queenCastle = "8/p7/8/8/8/8/8/R3K3 w KQkq -"
castletest = describe "castling" $ do
  it "must not be allowed kingside when kingrook moved" $ do
    let brd = allowedMoves ["h1h2", "a7a6", "h2h1", "a6a5"] $ unsafeFromFEN kingCastle in
      move "O-O" brd `must_eq` Left InvalidMove
  it "must not be allowed queenside when queenrook has moved" $ do
    let brd = allowedMoves ["a1a2", "a7a6", "a2a1", "a6a5"] $ unsafeFromFEN queenCastle in
      move "O-O-O" brd `must_eq` Left InvalidMove
  it "must not be allowed when king has moved" $ do
    let brd = allowedMoves ["e1e2", "a7a6", "e2e1"] $ unsafeFromFEN kingCastle in
      move "O-O" brd `must_eq` Left InvalidMove

pawnPromotionPos = "8/P7/8/8/8/8/8/8 w KQkq -"
pawnPromotionCheck = "8/2K3Pr/8/8/8/8/8/8 w KQkq -"
promotion = describe "promotion" $ do
  it "must be allowed when pawn on last row" $ do
    let Just brd = fromFEN pawnPromotionPos in
      case move "a7a8q" brd of
        Right b -> pieceAt 0 7 b == Just (Piece White Queen) &&
                  pieceAt 0 6 b == Nothing
        _ -> False
  it "must not be allowed when it causes check" $ do
    let Just brd = fromFEN pawnPromotionCheck in
      move "g7g8q" brd == Left CausesCheck

fentest = describe "fen" $ do
  it "must be equal to input fen" $ do
    toFEN (unsafeFromFEN pawnPromotionPos) `must_eq` pawnPromotionPos
    toFEN (unsafeFromFEN pawnPromotionCheck) `must_eq` pawnPromotionCheck
    toFEN (unsafeFromFEN checkMoveA) `must_eq` checkMoveA
    toFEN (unsafeFromFEN checkMoveB) `must_eq` checkMoveB
    toFEN (unsafeFromFEN kingCastle) `must_eq` kingCastle
    toFEN (unsafeFromFEN queenCastle) `must_eq` queenCastle
    toFEN (unsafeFromFEN staleMatePos) `must_eq` staleMatePos
    toFEN (unsafeFromFEN queenCastleCheckPosA) `must_eq` queenCastleCheckPosA
    toFEN (unsafeFromFEN queenCastleCheckPosB) `must_eq` queenCastleCheckPosB
    toFEN (unsafeFromFEN enpasPos) `must_eq` enpasPos

staleMatePos = "7k/8/6r1/4K3/6r1/3r1r2/8/8 w - -"
stalematetest = describe "stalemate" $ do
  it "should be detected" $ do
    stalemate White $ unsafeFromFEN staleMatePos

matePos = "3K2r1/6r1/8/8/8/8/8/2k5 w - -"
matetest = describe "mate" $ do
  it "should be detected" $ do
    mate White $ unsafeFromFEN matePos

enpasPos = "rnbqkbnr/ppp2ppp/8/3pP3/8/8/PPP1PPPP/RNBQKBNR w KQkq d6"
enpasTest = describe "read enpassant" $ do
  it "Must accept e5d6 on rnbqkbnr/ppp2ppp/8/3pP3/8/8/PPP1PPPP/RNBQKBNR w KQkq d6" $ do
    let Just brd = fromFEN enpasPos in
      valid "e5d6" brd

mulMovesExcept pcs lst = concat $ map (\x -> movesExcept x lst) pcs
movesExcept pc lst = (filter (not . flip elem lst) (allMoves pc))
allMoves pc = [pc ++ [(chr (x+97)), (intToDigit (y+1))] | x<-[0..7], y<-[0..7]]

tests = describe "The move validator" $ do
  begpos
  enumpos
  pawn
  rook
  knight
  bishop
  queen
  king
  kingcastletest
  queencastletest
  checkmoves
  castletest
  promotion
  fentest
  stalematetest
  matetest
  enpasTest

unsafeFromFEN = fromJust . fromFEN

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