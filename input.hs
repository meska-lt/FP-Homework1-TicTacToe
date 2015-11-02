module TicTacToe.Messages.SExpr
where

{-
message to find out a winner
board:
+-+-+-+
| |X| |
+-+-+-+
| | | |
+-+-+-+
|X| |O|
+-+-+-+
-}
message :: String
message = "(m \"0\" (m \"x\" 0 \"y\" 1 \"v\" \"x\") \"1\" (m \"x\" 2 \"y\" 2 \"v\" \"o\") \"2\" (m \"x\" 2 \"y\" 0 \"v\" \"x\"))"
