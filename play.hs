import System.IO(readFile)
import System.Exit(die)
import System.Environment(getArgs, getProgName)
import Data.Char(isLetter, isSpace, toLower, chr)
import qualified Data.Map.Strict as M
import Data.List(sort, sortBy); import Data.Tuple(swap)
import Chess; import Chess.FEN; import Gametest

specialInput inpt brd err
  | inpt == "moves"  = putStrLn . show $ moveList brd
  | null inpt        = putStrLn ""
  | head inpt == '!' = putStrLn . show $ map (parse [(x, y) | x <- [0..7], y <- [0..7]]) $ words (tail inpt)
  | otherwise        = putStrLn $ show err
    where 
      parse locs ""  = locs
      parse locs (c:[])
        | isLetter c = filter ((c==) . chr . (+97) . fst) locs
        | otherwise  = filter ((c==) . chr . (+49) . snd) locs
      parse locs (c:p:xs)
        | p == '*'  = parse (filter (\(x, y) -> isJust (pieceAt x y) && c == head . fromJust $ pieceAt x y) locs) xs
        | otherwise = [strToPos (c:p:[])]

play :: Board -> IO()
play board = do putStrLn $ show board
                mv <- getLine
                case move mv board of 
                  Right brd    -> play brd
                  Left mvError -> do
                    specialInput mv board mvError
                    play board

main :: IO ()
main = do args <- getArgs
          let board = Chess.FEN.defaultBoard
          play board
