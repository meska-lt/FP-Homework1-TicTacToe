module Parser
where

import Data.List.Extra
import TicTacToe.Messages.SExpr

type InternalMap = [(String, String)]

boardEdgeLength :: Int
boardEdgeLength = 3

data Player = X | O deriving (Show, Read, Eq)
type Position = (Int, Int)
type Board = [[Maybe Player]]

strToPlayer :: String -> Player
strToPlayer "+" = X
strToPlayer "x" = X
strToPlayer "X" = X
strToPlayer "o" = O
strToPlayer "O" = O
strToPlayer "0" = O

emptyBoard :: Board
emptyBoard = replicate boardEdgeLength $ replicate boardEdgeLength Nothing

replaceNth :: Int -> Maybe Player -> [Maybe Player] -> [Maybe Player]
replaceNth n newVal (head:tail)
     | n == 0 = newVal:tail
     | otherwise = head:replaceNth (n-1) newVal tail

findParam :: InternalMap -> String -> String -> String
findParam map param errorMsg =
    case lookup param map of
        Just val -> val
        Nothing -> error errorMsg

stripElem :: String -> String -> String -> String -> String
stripElem str elemPrefix elemPostfix errorMsg = 
    case stripPrefix elemPrefix str of
        Just rest -> case stripSuffix elemPostfix rest of
            Just rest -> rest
            Nothing -> error errorMsg
        Nothing -> error errorMsg

getMapContents :: String -> InternalMap -> InternalMap
getMapContents [] acc = acc
getMapContents str acc =
    let 
        item = takeWhile (/= ' ') str
        value = takeWhile (/= ' ') (drop (length item + 1) str)
        rest = drop (length item + length value + 2) str
    in reverse $ getMapContents rest ((item, value) : acc)

getMapElem :: String -> Maybe (String, String)
getMapElem [] = Nothing
getMapElem str =
    let 
        key = takeWhile (/= ')') str ++ ")"
        rest = drop (length key + 2) str
    in Just (key, rest)

parseList :: String -> [String] -> [String]
parseList [] acc = acc
parseList str acc =
    case getMapElem str of
        Just (key, rest) -> parseList rest (key : acc)
        Nothing -> error "Unknown error."

parseMaps :: [String] -> [InternalMap] -> [InternalMap]
parseMaps [] acc = acc
parseMaps (head:tail) acc =
    let
        num = takeWhile (/= '(') head
        denum = drop (length num) head
        striped = filter (/= '"') (stripElem denum "(m " ")" "Not a map.")
        parsed = getMapContents striped []
    in parseMaps tail (parsed : acc)

parseSExpr :: String -> [InternalMap]
parseSExpr str =
    let
        listContent = stripElem str "(m " ")" "Not a list."
        parsedData = parseMaps (parseList listContent []) []
    in parsedData

fillTheGrid :: [InternalMap] -> [Maybe Player] -> Board
fillTheGrid [] grid = chunksOf boardEdgeLength grid
fillTheGrid (x:xs) grid =
    let
        pos1 = read (findParam x "x" "x not defined.") :: Int
        pos2 = read (findParam x "y" "y not defined.") :: Int
        player = findParam x "v" "player not defined."
        index = boardEdgeLength * pos1 + pos2
        newGrid = replaceNth index (Just (strToPlayer player)) grid
    in fillTheGrid xs newGrid

getWinSeqs :: Board -> [[Maybe Player]]
getWinSeqs grid = horizontal ++ vertical ++ [fDiag, bDiag]
  where horizontal = grid
        vertical = transpose grid
        fDiag = zipWith (!!) (reverse grid) [0..]
        bDiag = zipWith (!!) grid [0..]

getWinner :: String -> String
getWinner map
    | winner X  = "Winner: X"
    | winner O  = "Winner: O"
    | otherwise = "Winner: there is none"
    where
        grid = fillTheGrid (parseSExpr map) (concat emptyBoard)
        winner :: Player -> Bool
        winner player = any (all (== Just player)) $ getWinSeqs grid