module Minimax( MinimaxTree(..)
              , moveList
              , score
              , boardOf
              , nextBoards
              , minimaxFrom
              ) where
import Chess.FEN
import Data.Either
import Data.Maybe
import Data.List
import Chess
import Control.Parallel.Strategies(using, parList, rseq, rpar)

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


data MinimaxTree = Root (Board) [MinimaxTree]
                     | Partial (Board) [MinimaxTree] (MinimaxTree)
                     | Final (Board) (MinimaxTree)

boardOf (Root brd _)      = brd
boardOf (Partial brd _ _) = brd
boardOf (Final brd _)     = brd

nextBoards :: Board -> [Board]
nextBoards brd = [(fixedMove x y x2 y2 brd) | (x,y,x2,y2) <- moveList brd]

minimaxFrom brd = root
    where root = Root brd (map (minimaxFrom' root) nextPositions)
          nextPositions = nextBoards brd
          minimaxFrom' parent brd'
            | null nnPositions = Final brd' parent
            | otherwise       = this
              where this = Partial brd' (map (minimaxFrom' this) nnPositions) parent
                    nnPositions = nextBoards brd'

displayLayers n (Root brd next)
  | n == 0 = return ()
  | otherwise = do putStr $ show brd
                   mapM (displayLayers (n-1)) next
                   putStrLn ""

displayLayers n (Partial brd next _)
  | n == 0 = return ()
  | otherwise = do putStr $ show brd
                   mapM (displayLayers (n-1)) next
                   putStrLn ""

displayLayers n (Final brd _) = putStrLn $ show brd

