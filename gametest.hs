import System.IO(readFile)
import System.Exit(die)
import System.Environment(getArgs, getProgName)
import Data.Char(isLetter, isSpace, toLower)
import qualified Data.Map.Strict as M
import Data.List(sort, sortBy); import Data.Tuple(swap)
import Chess; import Chess.FEN


play :: Board -> IO()
play board = do putStrLn $ show board
                mv <- getLine
                case move mv board of 
                  Right brd    -> play brd
                  Left mvError -> do 
                    putStrLn $ show mvError
                    play board

main :: IO ()
main = do args <- getArgs
          let board = Chess.FEN.defaultBoard
          play board
