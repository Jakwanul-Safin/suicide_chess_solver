# suicide_chess_solver

    A program to decide on the best moves in a game of suicide chess. Sucide chess is a variant of chess where each player’s goal is to lose all their pieces. 

The rules are the same as in normal chess except as follows:

1) Capture must be played if any captures are possible.
2) Kings are treated as normal pieces (no checks, checkmates, castling and pawns can promote to king)
3) Stalemates are a win for the statemated player

This algorithm is coded in Haskel and appropriately parallelized.
