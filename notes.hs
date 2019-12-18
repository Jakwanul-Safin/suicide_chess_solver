-- Non-working alpha beta
{-
ab' n brd = ab n (-1000) (1000) brd
ab n a b brd
    | n == 0 || length nb == 0 = a `max` (score brd * mult) `min` b
    | otherwise = f a nb
    where
        mult = if (turn brd == Black) then -1 else 1
        nb = nextBoards brd
        f a_ [] = a_
        f a_ (nb1:nbs)
            | a' >= b = a'
            | otherwise = f a' nbs
            where a' = - (ab (n-1) (-b) (-a) nb1)
-}

--solve :: Board -> Int

--nextLayer layer = layer >>= (\b -> [(fixedMove x y x2 y2 b) | (x,y,x2,y2) <- moveList b])

{-
negaMax depth brd
    | depth == 0 || abs (score brd) == 100 = score brd
    | otherwise = - minimum (map negaMax (depth-1)) $ map score $ nextBoards brd
-}

--negaMax :: (Num a) => a -> Board -> Int
--negaMax 0 brd = score brd

{-
moveList' brd 
    | length (captureMoves) > 0 = map (\((x1,y1),(x2,y2)) -> (x1,y1,x2,y2)) captureMoves 
    | otherwise = filter (okMove brd) 
                    [(x1,y1,x2,y2) | x1 <- [0..7], y1 <- [0..7], x2 <- [0..7], y2 <- [0..7]]
    where 
        captureMoves = forcedCapture (turn brd) brd
-}
