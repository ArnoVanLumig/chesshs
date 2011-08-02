module Chess(MoveError(..), Color(..), PieceType(..), Piece(..), Board(..), pieceAt, pieceAtStr, fromFEN, toFEN, move, check, mate, stalemate) where

import Control.Monad.Instances
import Array
import Data.Char
import qualified Data.List as L
import Data.Either
import Data.Maybe

data MoveError = WrongTurn | NoPiece | IsCheck | CausesCheck | InvalidMove | OverPiece | CapturesOwn deriving (Eq, Show)
data MoveType = RegularMove | KingMove | KingRookMove | QueenRookMove | DoublePawnMove | EnPassant deriving (Eq, Show)
data Color = Black | White deriving (Eq, Show)
data PieceType = Rook | Knight | Bishop | Queen | King | Pawn deriving (Eq, Show)
data Piece = Piece {clr :: Color, piece :: PieceType} deriving (Eq)

pcsList = [('r', Rook), ('n', Knight), ('b',Bishop),('q',Queen),('k', King),('p',Pawn)]
pieceType a = snd $ head $ filter (\(x,y) -> toLower a == x) pcsList
pieceName a = fst $ head $ filter(\(x,y) -> y == a) pcsList

instance Read Piece where
  readsPrec _ (pc:x) = [(Piece clr $ pieceType pc, x)] where
    clr = if isUpper pc then White else Black

instance Show Piece where
  show (Piece c t) = if c == White then [toUpper $ pieceName t] else [pieceName t]
  
data Board = Board { turn :: Color, castlingAvail :: String, enpassant :: Maybe (Int, Int), board :: Array (Int, Int) (Maybe Piece) } deriving (Eq)

remCastle rem brd = brd { castlingAvail = (castlingAvail brd) L.\\ rem }

instance Show Board where
  show b = unlines [ [ tos (board b ! (x,y)) | x<-[0..7] ] | y<-[0..7]] where
    tos p = fromMaybe ' ' (p >>= return . head . show)
             
fromFEN fen = readPosition $ words fen
  where readPosition (pieces:turn:castle:enpassant:_) = 
          Board (clr turn) castle enpas board where
            clr x = if x == "w" then White else Black
            enpas = if enpassant == "-" then Nothing else Just $ strToPos enpassant
            board = listArray ((0,0),(7,7)) (concat $ L.transpose $ map makeLine (reverse boardLines))
            boardLines = split pieces '/'
            makeLine ls = foldr ((++) . pcs) [] ls
            pcs a = if isDigit a then replicate (digitToInt a) Nothing else [Just (read [a])]

toFEN brd = pieces ++ " " ++ turnstr ++ " " ++ castString ++ " " ++ enpassantstr where
  pieces = unsplit (map fenline $ [ [ (board brd)!(j,7-i) | j<-[0..7]] | i<-[0..7]]) "/"
  turnstr = if turn brd == White then "w" else "b"
  enpassantstr = fromMaybe "-" (enpassant brd >>= \(x,y) -> return [chr (x+97), intToDigit y])
  castString = if castlingAvail brd == "" then "-" else castlingAvail brd
  fenline pcs = concatMap tos $ foldr com [] pcs where
    tos = either show show
    com a b = case a of
      Just x -> (Right x) : b
      Nothing -> case b of 
        ((Left n):xs) -> (Left (n+1)) : xs
        _ -> (Left 1) : b

otherColor x = if x == White then Black else White

isLeft (Left _) = True
isLeft _ = False

strToPos a = (ord (head a) - 97, digitToInt (head $ tail a) - 1)

split [] delim = [""]
split (c:cs) delim
   | c == delim = "" : rest
   | otherwise = (c : head rest) : tail rest
   where rest = split cs delim

unsplit [] delim = ""
unsplit (x:[]) delim = x
unsplit (x:xs) delim = x ++ delim ++ (unsplit xs delim)

pieceAtStr a b = let (x,y) = strToPos a in pieceAt x y b
pieceAt x y b = board b ! (x,y)
piecesOf clr brd = [ (x,y) | (x,y)<-(indices $ board brd), apprPiece $ pieceAt x y brd ] where
  apprPiece Nothing = False
  apprPiece (Just (Piece c p)) = c == clr

kingCoords clr brd = listToMaybe [ i | (i, pc) <- (assocs $ board brd), pc == Just (Piece clr King) ]

okMove x y x2 y2 brd = not $ isLeft $ moveAllowed x y x2 y2  brd

rookValid x y x2 y2 brd
  | x == 0 && y `elem` [0,7] = QueenRookMove
  | x == 7 && y `elem` [0,7] = KingRookMove
  | otherwise = RegularMove

justIf cond val = if cond then Just val else Nothing

validMove x y x2 y2 brd = pieceAt x y brd >>= \piece -> validMove' piece where
  validMove' (Piece _ Rook) = justIf ((x == x2 && y /= y2) || (x /= x2 && y == y2)) (rookValid x y x2 y2 brd)
  validMove' (Piece _ Knight) = justIf ((abs (x-x2) `elem` [1,2]) && (abs (y-y2) `elem` ([1,2] L.\\ [abs (x-x2)]))) RegularMove
  validMove' (Piece _ Bishop) = justIf (abs (x2 - x) == abs (y2 - y)) RegularMove
  validMove' (Piece _ King) = justIf ((abs (x-x2) <= 1) && (abs (y2 - y) <= 1)) KingMove
  validMove' (Piece _ Queen) = justIf (abs (x2 - x) == abs (y2 - y) ||
                                      ((x == x2 && y /= y2) || (x /= x2 && y == y2))) RegularMove
  validMove' (Piece White Pawn)
    | x2 == x && y2 == y + 1 = Just RegularMove -- single step ahead
    | x2 == x && y == 1 && y2 == y + 2 = Just DoublePawnMove -- double step ahead
    | (y2 == y+1 && abs (x2-x) == 1) && (pieceAt x2 y2 brd /= Nothing) = Just RegularMove -- capture
    | enpassant brd == Just (x2,y2) && (pieceAt x2 y brd /= Nothing) = Just EnPassant -- en passant
    | otherwise = Nothing
  validMove' (Piece Black Pawn) 
    | x2 == x && y2 == y-1 = Just RegularMove -- single step ahead
    | x2 == x && y == 6 && y2 == y-2 = Just DoublePawnMove -- double step ahead
    | (y2 == y-1 && abs (x2-x) == 1) && isJust (pieceAt x2 y2 brd) = Just RegularMove -- capture
    | enpassant brd == Just (x2,y2) && isJust (pieceAt x2 y brd) = Just EnPassant -- en passant
    | otherwise = Nothing

moveAllowed x y x2 y2 brd
  | isNothing $ pieceAt x y brd = Left NoPiece
  | owncolor /= turn brd = Left WrongTurn
  | pieceAtDest = Left CapturesOwn
  | pieceInPath = Left OverPiece
  | pawnIncorrectCapt = Left OverPiece
  | otherwise = case validMove x y x2 y2 brd of
    Nothing -> Left InvalidMove
    Just mv -> if check owncolor $ moveNoCheck x y x2 y2 mv brd
               then Left CausesCheck
               else Right mv
    where
      pieceInPath = case ownpiece of
        Piece _ Knight -> False
        _ -> let (dx, dy) = (signum (x2 - x), signum (y2 - y))
                 sqs
                   | dx == 0 = [(x,yy) | yy<-[y,y+dy..y2], yy /= y, yy/=y2]
                   | dy == 0 = [(xx,y) | xx<-[x,x+dx..x2], xx /= x, xx/=x2]
                   | otherwise = [(x+d*dx,y+d*dy) | d<-[1..(min (abs (y2-y)) (abs (x2-x)))-1]]
                 in any (\(x,y) -> isJust $ pieceAt x y brd) sqs
      pieceAtDest = case pieceAt x2 y2 brd of
        Just (Piece clr _) -> clr == owncolor
        _ -> False
      pawnIncorrectCapt = piece ownpiece == Pawn && x == x2 && (isJust $ pieceAt x2 y2 brd)
      owncolor = clr ownpiece
      ownpiece = fromJust $ pieceAt x y brd

castleAllowed brd side =  
  pieceAt 4 y brd == Just (Piece (turn brd) King) &&
  all (\x -> isNothing $ pieceAt x y brd) nopc &&
  all (\x -> not $ check (turn brd) $ moveNoCheck 4 y x y RegularMove brd) noCheck &&
  castAvail `elem` castlingAvail brd
    where
      x = if side == King then 7 else 0
      y = if turn brd == White then 0 else 7
      nopc = if side == King then [5,6] else [1,2,3]
      noCheck = if side == King then [5,6] else [2,3]
      castAvail = (if turn brd == White then id else toLower) (if side == King then 'K' else 'Q')

movePiece x y x2 y2 brd = removePiece x y $ putPiece x2 y2 (pieceAt x y brd) brd
setEnpassant x y brd = brd { enpassant = Just (x,y) }
resetEnpassant brd = brd {enpassant = Nothing }
swapTurn brd = brd { turn = otherColor $ turn brd }
putPiece x y pc brd = brd { board = (board brd) // [((x,y),pc)]}
removePiece x y = putPiece x y Nothing
castcase clr c = if clr == White then map toUpper c else map toLower c

check clr brd = case kingCoords clr brd of
  Just (kingX, kingY) -> any (\(x,y) -> okMove x y kingX kingY tmpbrd) otherPieces
    where otherPieces = piecesOf (otherColor clr) brd
          tmpbrd = brd {turn = otherColor clr}
  Nothing -> False

stalemate clr brd = case kingCoords clr brd of 
  Just (kx,ky) -> not $ any (\(x,y) -> okMove kx ky x y tmpbrd) (km kx ky)
  Nothing -> False
  where 
    km kx ky = [(x,y)| x<-[kx-1,kx,kx+1], y<-[ky-1,ky,ky+1], x>=0, y>=0, x<8, y<8]
    tmpbrd = brd {turn=otherColor clr}

mate clr brd = check clr brd && stalemate clr brd

castle brd side
  | castleAllowed brd side = Right (swapTurn $ resetEnpassant $ moveKing $ moveRook brd)
  | otherwise = Left InvalidMove
    where
      y = if turn brd == White then 0 else 7
      (rookFrom, rookTo) = if side == King then (7, 5) else (0, 2)
      (kingFrom, kingTo) = if side == King then (4, 6) else (4, 1)
      moveKing board = movePiece kingFrom y kingTo y board
      moveRook board = movePiece rookFrom y rookTo y board

updateCastlingAvail brd = if check (turn brd) brd then remCastle (castcase (turn brd) "kq") brd else brd

promote x y pc clr brd = case lookup (toLower pc) pcsList of
  Just pct -> Right $ putPiece x y (Just $ Piece clr pct) brd
  Nothing -> Left InvalidMove

moveNoCheck x y x2 y2 moveType brd = case moveType of
  KingRookMove -> let Piece clr _ = fromJust $ pieceAt x y brd in moveNoCheck x y x2 y2 RegularMove (remCastle (castcase clr "k") brd)
  QueenRookMove -> let Piece clr _ = fromJust $ pieceAt x y brd in moveNoCheck x y x2 y2 RegularMove (remCastle (castcase clr "q") brd)
  KingMove ->  let Piece clr _ = fromJust $ pieceAt x y brd in moveNoCheck x y x2 y2 RegularMove (remCastle (castcase clr "kq") brd)
  RegularMove -> swapTurn $ resetEnpassant $ movePiece x y x2 y2 brd
  DoublePawnMove -> swapTurn $ setEnpassant x2 ((y+y2) `div` 2) $ movePiece x y x2 y2 brd
  EnPassant -> swapTurn $ resetEnpassant $ movePiece x y x2 y2 $ removePiece x2 y brd

move' x y x2 y2 brd = moveAllowed x y x2 y2 brd >>= \movetype -> return (updateCastlingAvail $ moveNoCheck x y x2 y2 movetype brd)

move mv brd
  | mv == "0-0" = castle brd King
  | mv == "0-0-0" = castle brd Queen
  | length mv == 5 = move (init mv) brd >>= promote x2 y2 (last mv) (turn brd)
  | length mv == 4 = move' x y x2 y2 brd where
    (x,y) = strToPos (take 2 mv)
    (x2,y2) = strToPos (drop 2 mv)