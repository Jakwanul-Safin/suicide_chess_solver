import Data.Maybe
import System.Environment(getArgs)
import Chess; import Chess.FEN; import Minimax

play :: Board -> IO()
play brd = do putStrLn $ show brd
              mv <- getLine
              if mv == "best" 
              then putStrLn . show $ itDeep 1 6 brd
              else
                case move mv brd of 
                  Right nxt_brd    -> play nxt_brd
                  Left mvError -> do
                    putStrLn $ show mvError
              play brd

main :: IO ()
main = do args <- getArgs
          let brd = case args of
                        []    -> Chess.FEN.defaultBoard
                        fen -> fromJust . fromFEN $ unwords fen
          play brd
