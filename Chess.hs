-- | The main move validation module. FEN parsing code is in Chess.FEN and PGN parsing is in Chess.PGN
module Chess( MoveError(..)
            , Color(..)
            , PieceType(..)
            , Piece(..)
            , Board(..)
            , strToPos
            , pieceAt
            , pieceAtStr
            , move
            , moveSAN
            , check
            , mate
            , stalemate
            ) where

import           Control.Monad.Instances
import           Data.Array
import           Data.Char
import qualified Data.List as L
import           Data.Either
import           Data.Maybe

data MoveError = WrongTurn -- ^ It's not your turn
               | NoPiece -- ^ There is no piece at the "from" position
               | IsCheck -- ^ Your king is checked and this move doesn't solve that
               | CausesCheck -- ^ After this move your king would be checked
               | InvalidMove -- ^ This is not how that piece works
               | OverPiece -- ^ You cannot move over other pieces
               | CapturesOwn -- ^ This move captures one of your own pieces
               | NoParse -- ^ I don't understand what you mean
               deriving (Eq, Show)

data MoveType = RegularMove
              | KingMove -- ^ A move with the king
              | KingRookMove -- ^ A move with the king-side rook
              | QueenRookMove -- ^ A move with the queen-side rook
              | DoublePawnMove -- ^ A move where a pawn makes two steps
              | EnPassant -- ^ En-passant capture
              deriving (Eq, Show)

data Color = Black
           | White
           deriving (Eq, Show)

data PieceType = Rook
               | Knight
               | Bishop
               | Queen
               | King
               | Pawn
               deriving (Eq, Show)

data Piece = Piece { clr :: Color
                   , piece :: PieceType
                   } deriving (Eq)

data Board = Board { turn :: Color
                   , castlingAvail :: String
                   , enpassant :: Maybe (Int, Int)
                   , board :: Array (Int, Int) (Maybe Piece)
                   } deriving (Eq)

pcsList = [('r', Rook), ('n', Knight), ('b', Bishop), ('q', Queen), ('k', King), ('p', Pawn)]
pieceType a = snd $ head $ filter (\(x,y) -> toLower a == x) pcsList
pieceName a = fst $ head $ filter(\(x,y) -> y == a) pcsList

instance Read Piece where
  readsPrec _ (pc:x) = [(Piece clr $ pieceType pc, x)] where
    clr = if isUpper pc then White else Black

instance Show Piece where
  show (Piece c t) = if c == White then [toUpper $ pieceName t] else [pieceName t]

remCastle rem brd = brd { castlingAvail = (castlingAvail brd) L.\\ rem }

instance Show Board where
  show b = unlines [ [ tos (board b ! (x,y)) | x<-[0..7] ] | y<-[7,6..0]] where
    tos p = fromMaybe '.' (p >>= return . head . show)

otherColor x = if x == White then Black else White

posToStr (x,y) = [chr (x + 97), chr (y + 49)]

-- |Takes a position like "a5" and returns the coordinates (0,4)
strToPos :: String -> (Int, Int)
strToPos a = (ord (head a) - 97, digitToInt (head $ tail a) - 1)

charToRow a = digitToInt a - 1
charToCol a = ord a - 97

-- |What piece is currently at this position on the board?
pieceAtStr :: String -> Board -> Maybe Piece
pieceAtStr a b = let (x,y) = strToPos a in pieceAt x y b

-- |Like 'pieceAtStr', but with coordinates instead of a string
pieceAt :: Int -> Int -> Board -> Maybe Piece
pieceAt x y b = board b ! (x,y)

piecesOf clr brd = [ (x,y) | (x,y)<-(indices $ board brd), apprPiece $ pieceAt x y brd ] where
  apprPiece Nothing = False
  apprPiece (Just (Piece c p)) = c == clr

kingCoords clr brd = listToMaybe $ pieceCoords clr brd King

pieceCoords clr brd piece = [ i | (i, pc) <- (assocs $ board brd), pc == Just (Piece clr piece) ]

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
    | enpassant brd == Just (x2,y2) && (pieceAt x2 y brd /= Nothing) && abs (x2-x) == 1 && y2-y == 1 = Just EnPassant -- en passant
    | otherwise = Nothing
  validMove' (Piece Black Pawn)
    | x2 == x && y2 == y-1 = Just RegularMove -- single step ahead
    | x2 == x && y == 6 && y2 == y-2 = Just DoublePawnMove -- double step ahead
    | (y2 == y-1 && abs (x2-x) == 1) && isJust (pieceAt x2 y2 brd) = Just RegularMove -- capture
    | enpassant brd == Just (x2,y2) && isJust (pieceAt x2 y brd) && abs (x2-x) == 1 && y2-y == -1 = Just EnPassant -- en passant
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

-- |Is the player of the given colour check?
check :: Color -> Board -> Bool
check clr brd = case kingCoords clr brd of
  Just (kingX, kingY) -> any (\(x,y) -> okMove x y kingX kingY tmpbrd) otherPieces
    where otherPieces = piecesOf (otherColor clr) brd
          tmpbrd = brd {turn = otherColor clr}
  Nothing -> False

-- |Can the player of the given colour make any move?
stalemate :: Color -> Board -> Bool
stalemate clr brd = case kingCoords clr brd of
  Just (kx,ky) -> not $ any (\(x,y) -> okMove kx ky x y tmpbrd) (km kx ky)
  Nothing -> False
  where
    km kx ky = [(x,y)| x<-[kx-1,kx,kx+1], y<-[ky-1,ky,ky+1], x>=0, y>=0, x<8, y<8]
    tmpbrd = brd {turn = clr}

-- |Is the player with the given colour checkmate
mate :: Color -> Board -> Bool
mate clr brd = check clr brd && stalemate clr brd

castle brd side
  | castleAllowed brd side = Right (swapTurn $ resetEnpassant $ moveKing $ moveRook brd)
  | otherwise = Left InvalidMove
    where
      y = if turn brd == White then 0 else 7
      (rookFrom, rookTo) = if side == King then (7, 5) else (0, 3)
      (kingFrom, kingTo) = if side == King then (4, 6) else (4, 2)
      moveKing board = movePiece kingFrom y kingTo y board
      moveRook board = movePiece rookFrom y rookTo y board

promote x y pc clr brd = case lookup (toLower pc) pcsList of
  Just pct -> Right $ putPiece x y (Just $ Piece clr pct) brd
  Nothing -> Left InvalidMove

moveNoCheck x y x2 y2 moveType brd = case moveType of
  KingRookMove -> let Piece clr _ = fromJust $ pieceAt x y brd
                  in moveNoCheck x y x2 y2 RegularMove (remCastle (castcase clr "k") brd)
  QueenRookMove -> let Piece clr _ = fromJust $ pieceAt x y brd
                   in moveNoCheck x y x2 y2 RegularMove (remCastle (castcase clr "q") brd)
  KingMove ->  let Piece clr _ = fromJust $ pieceAt x y brd
               in moveNoCheck x y x2 y2 RegularMove (remCastle (castcase clr "kq") brd)
  RegularMove -> swapTurn $ resetEnpassant $ movePiece x y x2 y2 brd
  DoublePawnMove -> swapTurn $ setEnpassant x2 ((y+y2) `div` 2) $ movePiece x y x2 y2 brd
  EnPassant -> swapTurn $ resetEnpassant $ movePiece x y x2 y2 $ removePiece x2 y brd

move' x y x2 y2 brd = moveAllowed x y x2 y2 brd >>= \movetype -> return (moveNoCheck x y x2 y2 movetype brd)

-- | Perform a move on the board in coordinate notation like "e2e4", returning either the new board or an error
move :: [Char] -> Board -> Either MoveError Board
move mv brd
  | mv == "O-O" = castle brd King
  | mv == "O-O-O" = castle brd Queen
  | length mv == 5 = move (init mv) brd >>= promote x2 y2 (last mv) (turn brd)
  | length mv == 4 = move' x y x2 y2 brd
  | otherwise = Left NoParse where
    (x,y) = strToPos (take 2 mv)
    (x2,y2) = strToPos (drop 2 mv)

-- |Perform a move in SAN notation on the board and return either the new board or an error
moveSAN :: [Char] -> Board -> Either MoveError Board
moveSAN mv brd
  | mv' == "O-O" = move "O-O" brd
  | mv' == "O-O-O" = move "O-O-O" brd
  | not $ head mv' `elem` "PRNBKQ" = moveSAN ('P':mv') brd
  | last mv' `elem` "RNBQ" = moveSAN' (pieceType (head mv')) (init $ tail mv') (Just $ pieceType $ last mv') brd
  | otherwise = moveSAN' (pieceType (head mv')) (tail mv') Nothing brd
  where mv' = L.delete 'x' $ L.delete '+' $ L.delete '#' $ L.delete '=' mv

moveSAN' piece mv promo brd
  | length mv == 2 = -- piece and target square given
    let potPcs = pieceCoords' piece in
    case rights $ map (flip move brd) (potentialMoves potPcs) of
      [x] -> Right x
      _   -> Left NoParse
  | head mv `elem` "0123456789" = -- starting rank given
    let potPcs = filter (\(_,y) -> y == charToRow (head mv)) (pieceCoords' piece) in
    case rights $ map (flip move brd) (potentialMoves potPcs) of
      [x] -> Right x
      _   -> Left NoParse
  | otherwise = -- starting file given
    let potPcs = filter (\(x,_) -> x == charToCol (head mv)) (pieceCoords' piece) in
    case rights $ map (flip move brd) (potentialMoves potPcs) of
      [x] -> Right x
      _   -> Left NoParse
  where pieceCoords' = pieceCoords (turn brd) brd
        promoStr = case promo of
          Just p -> [toUpper $ pieceName p]
          Nothing -> ""
        potentialMoves
          | length mv == 2 = map (\x -> posToStr x ++ mv ++ promoStr)
          | length mv == 3 = map (\x -> posToStr x ++ tail mv ++ promoStr)
