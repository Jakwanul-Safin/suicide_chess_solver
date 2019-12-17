import Chess.FEN
import Data.Either
import Data.Maybe
import Data.Array
import Data.Char
import qualified Data.List as L
import Data.Tree
import Chess

pos = fromJust $ fromFEN "8/8/8/4n3/6q1/8/5K2/8 w - - 0 1"
p2 = fixedMove 5 1 5 2 pos
p4 = fromJust $ fromFEN "8/8/4nn2/8/8/4K3/8/8 b - - 0 2"
p5 = nextBoards p4 !! 2
p6 = nextBoards p5 !! 0
p7 = nextBoards p6 !! 0

-- negaMax 6 pt -> takes ~2 minutes, solves correctly.
pt = fromJust $ fromFEN "2b2b2/1p2p3/4k3/8/3qP3/r7/5P2/2n1K3 w - - 0 20"

okMove brd (x,y,x2,y2) = not $ isLeft $ moveAllowed x y x2 y2 brd

instance Ord Board where
    b1 `compare` b2 = (score b1) `compare` (score b2)

fixedMove x y x2 y2 brd = case move' x y x2 y2 brd of
    Right new_brd -> new_brd
    Left error -> defaultBoard

moveList brd
  | null caps = empty
  | otherwise  = caps 
    where 
      empty = [(x, y, fst z, snd z) | x <- [0..7], y <- [0..7], z <- movesFrom x y brd]
      caps = filter (\(x, y, x2, y2) -> isJust $ pieceAt x2 y2 brd) empty  

moveList' brd 
    | length (captureMoves) > 0 = map (\((x1,y1),(x2,y2)) -> (x1,y1,x2,y2)) captureMoves 
    | otherwise = filter (okMove brd) 
                    [(x1,y1,x2,y2) | x1 <- [0..7], y1 <- [0..7], x2 <- [0..7], y2 <- [0..7]]
    where 
        captureMoves = forcedCapture (turn brd) brd

score brd 
    | blackScore == 0 = -100
    | whiteScore == 0 = 100
    | otherwise = blackScore - whiteScore
    where
        blackScore = length (piecesOf Black brd) 
        whiteScore = length (piecesOf White brd)

nextBoards brd = [(fixedMove x y x2 y2 brd) | (x,y,x2,y2) <- moveList brd]

nextLayer layer = layer >>= (\b -> [(fixedMove x y x2 y2 b) | (x,y,x2,y2) <- moveList b])

{-
negaMax depth brd
    | depth == 0 || abs (score brd) == 100 = score brd
    | otherwise = - minimum (map negaMax (depth-1)) $ map score $ nextBoards brd
-}

--negaMax :: (Num a) => a -> Board -> Int
--negaMax 0 brd = score brd
negaMax n brd
    | n == 0 || length nb == 0 = score brd * mult
    | otherwise = -minimum (map (negaMax (n-1)) nb)
    where
        nb = nextBoards brd
        mult
            | turn brd == Black = -1
            | turn brd == White = 1
