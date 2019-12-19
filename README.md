# suicide_chess_solver

    A program to decide on the best moves in a game of suicide chess. Sucide chess is a variant of chess where each player’s goal is to lose all their pieces. 

The rules are the same as in normal chess except as follows:

1) Capture must be played if any captures are possible.
2) Kings are treated as normal pieces (no checks, checkmates, castling and pawns can promote to king)
3) Stalemates are a win for the statemated player

This algorithm is coded in Haskel and appropriately parallelized.


In order to test the algorithm compile main.hs with the command

    stack ghc -- -O2 -threaded -rtsopts -eventlog main.hs -o main

and run main with the args position and [n], where n is the number of test cases to run.

Addionally, in order to play suicide chess compile play.hs with
    
    stack ghc -- -O2 play.hs -o play

and run play either with no args for a default board or a fen of the desired starting board.
Moves are given in standard chess notion, i.e. "e2e4", or "best" can be run to get a computer suggestion.
