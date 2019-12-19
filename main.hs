import Data.Maybe
import Chess; import Chess.FEN
import Minimax 
import Control.Parallel.Strategies(using, parList, rseq)

import System.Environment(getArgs, getProgName)
import System.IO.Error(catchIOError, isUserError, isDoesNotExistError,
    ioeGetFileName, isPermissionError)
import System.Exit(die)

-- Main entry point. Solves board, returns score (White wins) + best first move
solve :: Board -> (Int, [Char], Int)
solve brd = itDeep 1 8 brd                    

-- Formats the result into something readable
parse :: (Int, [Char], Int) -> [Char]
parse (scr, mv, qmv)
  | scr == 100 
      && odd qmv  = intro ++ ", White highly favored," ++ conc
  | scr == 100 = intro ++ ", Black highly favored," ++ conc
  | scr > 0    = intro ++ ", White favored (" ++ show scr ++ 
                      ")" ++ conc
  | otherwise     = intro ++ ", Black favored (" ++ show scr ++ 
                      ")" ++ conc
    where 
      intro = "Best move: "     ++ show mv
      conc  = " search depth: " ++ show qmv

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
