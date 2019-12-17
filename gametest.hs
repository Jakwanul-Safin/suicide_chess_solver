import Chess.FEN
import Data.Either
import Data.Maybe
import Control.Monad.Instances
import Data.Array
import Data.Char
import qualified Data.List as L
import Data.Tree
import Chess

pos = fromJust $ fromFEN "r3k2r/B1p2ppp/8/2bp4/3qP1Q1/2NP4/PPP2PPP/R3K2R w KQkq - 0 1"

okMove brd (x,y,x2,y2) = not $ isLeft $ moveAllowed x y x2 y2 brd

instance Ord Board where
    b1 `compare` b2 = (score b1) `compare` (score b2)

fixedMove x y x2 y2 brd = case move' x y x2 y2 brd of
    Right new_brd -> new_brd
    Left error -> defaultBoard

moveList brd 
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
negaMax 0 brd = score brd
negaMax n brd = - minimum (map (negaMax (n-1)) (nextBoards brd))

