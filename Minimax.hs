module Minimax( MinimaxTree(..)
              , moveList
              , score
              , boardOf
              , next
              , nextBoards
              , minimaxFrom
              , fromRoot
              , negaMax
              , itDeep
              ) where

import Data.Maybe
import Data.List
import Chess
import Control.Parallel.Strategies(using, parList, rseq)

-- Get all possible moves
moveList :: Board -> [(Int, Int, Int, Int)]
moveList brd
  | null caps = empty
  | otherwise  = caps 
    where 
      empty = [(x, y, fst z, snd z) | x <- [0..7], y <- [0..7], z <- movesFrom x y brd]
      caps = filter (\(_, _, x2, y2) -> isJust $ pieceAt x2 y2 brd) empty  

{- Data structure encoding minimax tree
 - Root is starting position
 - Parital is a node
 - Final is a leaf
 -}
data MinimaxTree = Root (Board) [MinimaxTree]
                 | Partial (Board) [MinimaxTree] (MinimaxTree)
                 | Final (Board) (MinimaxTree)

boardOf :: MinimaxTree -> Board
boardOf (Root brd _)      = brd
boardOf (Partial brd _ _) = brd
boardOf (Final brd _)     = brd

next :: MinimaxTree -> [MinimaxTree]
next (Root _ nxt)      = nxt
next (Partial _ nxt _) = nxt
next _                 = error("Next called on leaf")

nextBoards :: Board -> [Board]
nextBoards brd = map (\(x, y, x2, y2) -> fixedMove x y x2 y2) $ moveList brd
    where 
      fixedMove x y x2 y2 = case move' x y x2 y2 brd of
          Right new_brd -> new_brd
          Left _        -> error("Invalid move given")

minimaxFrom' :: MinimaxTree -> Board -> MinimaxTree
minimaxFrom' parent brd
    | null nextPos  = Final brd parent
    | otherwise     = partial
      where 
        partial = Partial brd (map (minimaxFrom' partial) nextPos) parent
        nextPos = nextBoards brd

--Builds the minimax tree from a given node
minimaxFrom :: Board -> MinimaxTree
minimaxFrom brd = root
    where 
      root = Root brd (map (minimaxFrom' root) nextPositions)
      nextPositions = nextBoards brd

fromRoot :: MinimaxTree -> [Board]
fromRoot (Root brd _)           = brd:[]
fromRoot (Partial brd _ parent) = brd:(fromRoot parent)
fromRoot (Final brd parent)     = brd:(fromRoot parent)

-- Static evaluation. 100 = win.
score :: MinimaxTree -> Int
score (Final brd _) = if turn brd == White then 100 else -100
score mmTree        = max (-99) $ min 99 heuristic
    where
      brd       = boardOf mmTree
      mult      = if turn brd == White then 1 else -1
      qBlack    = length $ piecesOf Black brd
      qWhite    = length $ piecesOf White brd
      flexMe    = length (next mmTree)
      flexOp    = length (moveList brd{turn = 
                    if turn brd == White then Black else White})
      heuristic = 10 * (qBlack `div` qWhite - 1) + mult * (flexMe - flexOp)
 
-- Simplified minimax
negaMax :: Int -> MinimaxTree -> Int
negaMax _ (Final brd _)        = if (turn brd == White) then 100 else -100
negaMax n mmTree
  | n == 0    = mult * (score mmTree)
  | otherwise = -minimum (map (negaMax (n-1)) nb `using` parList rseq) 
    where 
      nb = take 4 $ sortOn (negate . (*mult) . score ) $ next mmTree
      brd = boardOf mmTree
      mult = if (turn brd == White) then 1 else -1

-- Minimax that stores the best move.
miniMaxWithMoves :: Int -> MinimaxTree -> (Int, [Char])
miniMaxWithMoves n mmTree =  
    maximumBy (\(x,_) (y,_) -> compare x y) $ 
    zip results (map stringMove $ moveList $ boardOf mmTree)
    where 
      stringMove (x1,y1,x2,y2) = (posToStr (x1,y1)) ++ (posToStr (x2,y2))
      nb = take 8 $ sortOn (negate . score ) $ next mmTree 
      results = map (negate . (negaMax $ n-1)) nb
                        `using` parList rseq

-- Calls minimax with increasing depth until answer.
itDeep :: Int -> Int -> Board -> (Int, [Char], Int)
itDeep depth limit brd
  | abs (fst results) == 100 || depth > limit = ret
  | otherwise = itDeep (depth+1) limit brd
    where
      ret = (fst results, snd results, depth)
      results = miniMaxWithMoves depth $ minimaxFrom brd 

