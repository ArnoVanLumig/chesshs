This is a simple library for checking which moves are allowed on a given chess position. As far as I know, all chess rules have been implemented and work correctly.

The API is quite simple. To create a chess board use the `fromFEN` function, which loads a board from a FEN string (see http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation). You can then perform moves on the board by using the `move` function which takes a string of the form "b2b3" for regular moves, "0-0" for a kingside castle, "0-0-0" for a queenside castle, and "a7a8q" for promotion to a queen.

The code is quite messy and lacks documentation, so please take a look at ChessTest.hs to see how to use it.