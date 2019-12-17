-- ORIGINAL CODE IS FROM: chesshs hackage library, Copyright (c)2011, Arno van Lumig

-- | The main move validation module. FEN parsing code is in Chess.FEN and PGN parsing is in Chess.PGNi
module Chess( MoveError(..)
            , Color(..)
            , PieceType(..)
            , Piece(..)
            , Board(..)
            , strToPos
            , pieceAt
            , pieceAtStr
            , move
            , move'
            , piecesOf
            , moveSAN
            , stalemate
            , forcedCapture
            , validMove
            , moveAllowed
            , movesFrom
            ) where

import           Data.Array
import           Data.Char
import qualified Data.List as L
import           Data.Either
import           Data.Maybe

data MoveError = WrongTurn -- ^ It's not your turn
               | NoPiece -- ^ There is no piece at the "from" position
               | CaptureRequired -- ^ A capture is possible but not taken
               | InvalidMove -- ^ This is not how that piece works
               | OverPiece -- ^ You cannot move over other pieces
               | CapturesOwn -- ^ This move captures one of your own pieces
               | NoParse -- ^ I don't understand what you mean
               deriving (Eq, Show)

data MoveType = RegularMove
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

pieceCoords clr brd piece = [ i | (i, pc) <- (assocs $ board brd), pc == Just (Piece clr piece) ]

okMove x y x2 y2 brd = not $ isLeft $ moveAllowed x y x2 y2  brd

justIf cond val = if cond then Just val else Nothing

validMove x y x2 y2 brd = pieceAt x y brd >>= \piece -> validMove' piece where
  validMove' (Piece _ Rook) = justIf ((x == x2 && y /= y2) || (x /= x2 && y == y2)) RegularMove
  validMove' (Piece _ Knight) = justIf ((abs (x-x2) `elem` [1,2]) && (abs (y-y2) `elem` ([1,2] L.\\ [abs (x-x2)]))) RegularMove
  validMove' (Piece _ Bishop) = justIf (abs (x2 - x) == abs (y2 - y)) RegularMove
  validMove' (Piece _ King) = justIf ((abs (x-x2) <= 1) && (abs (y2 - y) <= 1)) RegularMove
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

movesFrom x y brd 
  | isNothing piece          = []
  | clr ownpiece /= turn brd = []
  | otherwise                = movesFrom' ownpiece
    where
      piece = pieceAt x y brd
      ownpiece = fromJust piece
      owncolor = clr ownpiece
 
      mvFilter mvs
        | null right || not (opponentAt x2 y2) = left 
        | otherwise                            = head right :[]
        where (left, right) = span (\(x2, y2) -> isNothing $ pieceAt x2 y2 brd) mvs
              (x2, y2) = head right

      opponentAt x2 y2 = isJust target && clr (fromJust target) /= owncolor 
        where target = pieceAt x2 y2 brd

      rookFrom xx yy = (mvFilter $ zip [xx-1, xx-2..0] [yy, yy..]) ++
                       (mvFilter $ zip [xx+1, xx+2..7] [yy, yy..]) ++
                       (mvFilter $ zip [xx, xx..] [yy-1, yy-2..0]) ++
                       (mvFilter $ zip [xx, xx..] [yy+1, yy+2..7])

      bishopFrom xx yy = (mvFilter $ zip [xx-1, xx-2..0] [yy-1, yy-2..0]) ++
                         (mvFilter $ zip [xx+1, xx+2..7] [yy+1, yy+2..7]) ++
                         (mvFilter $ zip [xx+1, xx+2..7] [yy-1, yy-2..0]) ++
                         (mvFilter $ zip [xx-1, xx-2..0] [yy+1, yy+2..7])


      inBounds x2 y2 = 0 <= x2 && x2 < 8 && 0 <= y2 && y2 < 8 

      movesFrom' (Piece _ Rook)   = rookFrom x y
      movesFrom' (Piece _ Bishop) = bishopFrom x y
      movesFrom' (Piece _ Queen)  = rookFrom x y ++ bishopFrom x y
      movesFrom' (Piece _ Knight)
        | null caps = empty
        | otherwise = caps
          where 
            locs = filter (uncurry inBounds) [(x+2, y+1), (x+1, y+2), (x-2, y+1), (x-1, y+2), (x+2, y-1), (x+1, y-2), (x-2, y-1), (x-1, y-2)]
            caps = filter (\(x2, y2) -> opponentAt x2 y2) locs
            empty = filter (\(x2, y2) -> isNothing $ pieceAt x2 y2 brd) locs 

      movesFrom' (Piece _ King)
        | null caps = empty
        | otherwise = caps
          where 
            locs = filter (uncurry inBounds) [(x+1, y+1), (x, y+1), (x-1, y+1), (x+1, y), (x-1, y), (x+1, y-1), (x, y-1), (x-1, y-1)]
            caps = filter (\(x2, y2) -> opponentAt x2 y2) locs
            empty = filter (\(x2, y2) -> isNothing $ pieceAt x2 y2 brd) locs 

      movesFrom' (Piece White Pawn)
        | not $ null captures = captures
        | y == 7 || isJust (pieceAt x (y + 1) brd) = []
        | y == 1 && isNothing (pieceAt x (y + 2) brd) = [(x, y + 1), (x, y + 2)]
        | otherwise = [(x, y + 1)]
        where
          enpassantCaps = filter (\(x2, y2) -> enpassant brd == Just (x2, y2)) $ filter (uncurry inBounds) [(x + 1, y), (x - 1, y)]
          captures = filter (\(x2, y2) -> opponentAt x2 y2) (enpassantCaps ++ filter (uncurry inBounds) [(x + 1, y + 1), (x - 1, y + 1)])
 
      movesFrom' (Piece Black Pawn)
        | not $ null captures = captures
        | y == 0 || isJust (pieceAt x (y - 1) brd) = []
        | y == 6 && isNothing (pieceAt x (y - 2) brd) = [(x, y - 1), (x, y - 2)]
        | otherwise = [(x, y - 1)]
        where
          enpassantCaps = filter (\(x2, y2) -> enpassant brd == Just (x2, y2)) $ filter (uncurry inBounds) [(x - 1, y), (x + 1, y)]
          captures = filter (\(x2, y2) -> opponentAt x2 y2) (enpassantCaps ++ filter (uncurry inBounds) [(x - 1, y - 1), (x + 1, y - 1)])


moveAllowed x y x2 y2 brd
  | isNothing $ pieceAt x y brd = Left NoPiece
  | owncolor /= turn brd = Left WrongTurn
  | pieceAtDest = Left CapturesOwn
  | pieceInPath = Left OverPiece
  | notCapture  = Left CaptureRequired
  | pawnIncorrectCapt = Left OverPiece
  | otherwise = case validMove x y x2 y2 brd of
    Nothing -> Left InvalidMove
    Just mv -> Right mv
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
      notCapture = case pieceAt x2 y2 brd of
        Just (Piece clr _) -> False
        _ -> not . null $ forcedCapture owncolor brd
      pawnIncorrectCapt = piece ownpiece == Pawn && x == x2 && (isJust $ pieceAt x2 y2 brd)
      owncolor = clr ownpiece
      ownpiece = fromJust $ pieceAt x y brd

movePiece x y x2 y2 brd = removePiece x y $ putPiece x2 y2 (pieceAt x y brd) brd
setEnpassant x y brd = brd { enpassant = Just (x,y) }
resetEnpassant brd = brd {enpassant = Nothing }
swapTurn brd = brd { turn = otherColor $ turn brd }
putPiece x y pc brd = brd { board = (board brd) // [((x,y),pc)]}
removePiece x y = putPiece x y Nothing
castcase clr c = if clr == White then map toUpper c else map toLower c

-- |Which pieces can a given player capture?

forcedCapture :: Color -> Board -> [((Int, Int), (Int, Int))]
forcedCapture clr brd = filter canCapture pairs
    where
        pieces = piecesOf clr brd 
        otherPieces = piecesOf (otherColor clr) brd
        pairs = [(p1,p2) | p1 <- pieces, p2 <- otherPieces]
        canCapture ((x1, y1), (x2, y2)) = okMove x1 y1 x2 y2 brd

-- |Can the player of the given colour make any move?
stalemate :: Color -> Board -> Bool
stalemate clr brd = False

{-
case kingCoords clr brd of
  Just (kx,ky) -> not $ any (\(x,y) -> okMove kx ky x y tmpbrd) (km kx ky)
  Nothing -> False
  where
    km kx ky = [(x,y)| x<-[kx-1,kx,kx+1], y<-[ky-1,ky,ky+1], x>=0, y>=0, x<8, y<8]
    tmpbrd = brd {turn = clr}
-}

promote x y pc clr brd = case lookup (toLower pc) pcsList of
  Just pct -> Right $ putPiece x y (Just $ Piece clr pct) brd
  Nothing -> Left InvalidMove

moveNoCheck x y x2 y2 moveType brd = case moveType of
  RegularMove -> swapTurn $ resetEnpassant $ movePiece x y x2 y2 brd
  DoublePawnMove -> swapTurn $ setEnpassant x2 ((y+y2) `div` 2) $ movePiece x y x2 y2 brd
  EnPassant -> swapTurn $ resetEnpassant $ movePiece x y x2 y2 $ removePiece x2 y brd

move' x y x2 y2 brd = moveAllowed x y x2 y2 brd >>= \movetype -> return (moveNoCheck x y x2 y2 movetype brd)

-- | Perform a move on the board in coordinate notation like "e2e4", returning either the new board or an error
move :: [Char] -> Board -> Either MoveError Board
move mv brd
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
