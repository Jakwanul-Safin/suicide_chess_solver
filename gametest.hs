import Chess.FEN
import Data.Either
import Data.Maybe
import Data.List
import Chess
import Control.Parallel.Strategies(using, parList, rseq, rpar)
import Minimax 

import System.Environment(getArgs, getProgName)
import System.IO.Error(catchIOError, isUserError, isDoesNotExistError,
    ioeGetFileName, isPermissionError)
import System.Exit(die)

-- Main entry point. Solves board, returns score (White wins) + best first move
solve :: Board -> Maybe (Int, [Char], Int)
solve brd = itDeep 1 8 brd                    

-- Formats the result into something readable
parse :: Maybe (Int, [Char], Int) -> [Char]
parse Nothing = "Couldn't find a winning position in under 4 moves" 
parse (Just (score, move, num_moves)) = "Best move: " ++ show move ++ result ++
                                 " in " ++ show num_moves ++ " moves"
    where result = if odd num_moves then ", white wins" else ", black wins"

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
