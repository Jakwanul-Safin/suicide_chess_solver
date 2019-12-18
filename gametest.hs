import Chess.FEN
import Data.Either
import Data.Maybe
import Data.List
import Chess

pos = fromJust $ fromFEN "8/8/8/4n3/6q1/8/5K2/8 w - - 0 1"
-- negaMax 6 pt -> takes ~20 seconds, solves correctly.
pt = fromJust $ fromFEN "2b2b2/1p2p3/4k3/8/3qP3/r7/5P2/2n1K3 w - - 0 20" -- Mate in 3
pt3 = fromJust $ fromFEN "8/5r2/8/5p2/6N1/3Q4/4K3/8 w - - 0 1" -- Mate in 3
pt4 = fromJust $ fromFEN "8/5r2/8/5p2/6N1/3QP3/4K3/8 w - - 0 1" -- Mate in 4
-- negaMax 8 pt4 -> ~ 120 seconds.
pt5 = fromJust $ fromFEN "8/8/8/8/4r2p/8/2P3K1/8 w - - 2 28" -- Mate in 2


okMove brd (x,y,x2,y2) = not $ isLeft $ moveAllowed x y x2 y2 brd

fixedMove x y x2 y2 brd = case move' x y x2 y2 brd of
    Right new_brd -> new_brd
    Left _ -> defaultBoard

-- Get all possible moves
moveList :: Board -> [(Int, Int, Int, Int)]
moveList brd
  | null caps = empty
  | otherwise  = caps 
    where 
      empty = [(x, y, fst z, snd z) | x <- [0..7], y <- [0..7], z <- movesFrom x y brd]
      caps = filter (\(x, y, x2, y2) -> isJust $ pieceAt x2 y2 brd) empty  

-- Static evaluation. 100 = win.
score :: Board -> Int
score brd 
    | blackScore == 0 = -100
    | whiteScore == 0 = 100
    | otherwise = blackScore - whiteScore
    where
        blackScore = length (piecesOf Black brd) 
        whiteScore = length (piecesOf White brd)

-- Get the list of all next possible board states
nextBoards :: Board -> [Board]
nextBoards brd = [(fixedMove x y x2 y2 brd) | (x,y,x2,y2) <- moveList brd]

-- Simplified minimax
negaMax :: Int -> Board -> Int
negaMax n brd
    | n == 0 || length nb == 0 = score brd * mult
    | otherwise = -minimum (map (negaMax (n-1)) nb)
    where
        nb = nextBoards brd
        mult = if (turn brd == Black) then -1 else 1

-- Minimax that stores the best move.
miniMaxWithMoves :: Int -> Board -> (Int, [Char])
miniMaxWithMoves n brd =  maximumBy (\(x,_) (y,_) -> compare x y) $ 
                                   zip (results brd)
                                   (map stringMove $ moveList brd)
                          where
                            stringMove (x1,y1,x2,y2) = (posToStr (x1,y1)) ++ (posToStr (x2,y2))
                            results brd = map negate $ map (negaMax (n-1)) $ nextBoards brd

-- Calls minimax with increasing depth until answer.
itDeep :: Int -> Int -> Board -> Maybe (Int, [Char], Int)
itDeep depth limit brd
    | fst results /= 100 && depth <= limit = itDeep (depth+1) limit brd
    | depth > limit = Nothing
    | otherwise = Just (fst results, snd results, depth `div` 2)
    where results = miniMaxWithMoves depth brd 

-- Main entry point. Solves board, returns score (White wins) + best first move
solve :: Board -> Maybe (Int, [Char], Int)
solve brd = itDeep 1 8 brd                    

-- Formats the result into something readable
parse :: Maybe (Int, [Char], Int) -> [Char]
parse Nothing = "Couldn't find a winning position in under 4 moves" 
parse (Just (score, move, num_moves)) = "Best move: " ++ show move ++ result ++
                                 " in " ++ show num_moves ++ " moves"
    where result = if score == 100 then ", white wins" else "ERROR"

main :: IO ()
main = do
    -- This is a decently good test suite. More notes above.
    let positions = [pos, pt, pt3, pt5, pt4]
    let results = map parse $ map solve positions
    sequence_ $ map putStrLn results
