import Chess.FEN
import Data.Either
import Data.Maybe
import Data.List
import Chess
import Control.Parallel.Strategies(using, parList, rseq, rpar)

import System.Environment(getArgs, getProgName)
import System.IO.Error(catchIOError, isUserError, isDoesNotExistError,
    ioeGetFileName, isPermissionError)
import System.Exit(die)


pos = fromJust $ fromFEN "8/8/8/4n3/6q1/8/5K2/8 w - - 0 1" -- Mate in 1
pt = fromJust $ fromFEN "2b2b2/1p2p3/4k3/8/3qP3/r7/5P2/2n1K3 w - - 0 20" -- Mate in 3
pt3 = fromJust $ fromFEN "8/5r2/8/5p2/6N1/3Q4/4K3/8 w - - 0 1" -- Mate in 3
pt4 = fromJust $ fromFEN "8/5r2/8/5p2/6N1/3QP3/4K3/8 w - - 0 1" -- Mate in 4
pt5 = fromJust $ fromFEN "8/8/8/8/4r2p/8/2P3K1/8 w - - 2 28" -- Mate in 2
pt6 = fromJust $ fromFEN "8/1n3k2/4p3/7p/4r3/7K/2P5/3R4 w - - 2 28" -- Mate in 3

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
  | null $ mvList = mult * 100
  | otherwise = 5 * (length (piecesOf Black brd) `div` length (piecesOf White brd) - 1) 
                    + mult * length mvList
    where
      mult   = if turn brd == White then 1 else -1
      mvList = moveList brd

-- Get the list of all next possible board states
nextBoards :: Board -> [Board]
nextBoards brd = [(fixedMove x y x2 y2 brd) | (x,y,x2,y2) <- moveList brd]

-- Simplified minimax
negaMax :: Int -> Board -> Int
negaMax n brd
    | null nb || n == 0 = mult * (score brd)
    | otherwise         = -minimum (map (negaMax (n-1)) nb `using` parList rseq)
    where nb = take 4 $ sortOn (negate . (*mult) . score) $ nextBoards brd
          mult = if (turn brd == White) then 1 else -1

-- Minimax that stores the best move.
miniMaxWithMoves :: Int -> Board -> (Int, [Char])
miniMaxWithMoves n brd =  maximumBy (\(x,_) (y,_) -> compare x y) $ 
                                   zip (results brd)
                                   (map stringMove $ moveList brd)
                          where
                            stringMove (x1,y1,x2,y2) = (posToStr (x1,y1)) ++ (posToStr (x2,y2))
                            nb = take 8 $ sortOn (negate . score) $ nextBoards brd 
                            results brd = map negate (map (negaMax (n-1)) (nextBoards brd)
                                          `using` parList rseq)

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

main:: IO()
main = do [filename, cases] <- getArgs
          contents <- readFile filename
          let brds = map (fromJust . fromFEN) . take (read cases) $ lines contents
              results = map parse (map solve brds `using` parList rseq)
          sequence_ $ map putStrLn results

  `catchIOError` \e -> do
    pn <- getProgName 
    die $ case ioeGetFileName e of
      Just fn | isDoesNotExistError e -> fn ++ ": no such file"
              | isPermissionError e   -> fn ++ ": Permission denied"
      _       | isUserError e         -> "Usage: " ++ pn ++
                  " <filename> <# of test cases>"
              | otherwise             -> show e
