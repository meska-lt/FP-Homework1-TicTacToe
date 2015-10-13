module Parser
where

import Data.List.Extra
import TicTacToe.Messages.Scala

type InternalMap = [(String, String)]
-- Size of the grid
n :: Int
n = 3

data Player = X | O deriving (Show, Read, Eq)
type Marking = Maybe Player
type Position = (Int, Int)

type Grid = [[Marking]]

strToPlayer :: String -> Player
strToPlayer "x" = X
strToPlayer "X" = X
strToPlayer "o" = O
strToPlayer "O" = O
strToPlayer "0" = O

-- An empty grid of size n x n.
emptyGrid :: Grid
emptyGrid = replicate n $ replicate n Nothing

replaceNth :: Int -> Marking -> [Marking] -> [Marking]
replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs

findParam :: InternalMap -> String -> String -> String
findParam map param errorMsg =
    case lookup param map of
        Just val -> val
        Nothing -> error errorMsg

stripElem :: String -> String -> String -> String
stripElem str elemPrefix errorMsg = 
    case stripPrefix (elemPrefix ++ "(") str of
        Just rest -> case stripSuffix ")" rest of
            Just rest -> rest
            Nothing -> error errorMsg
        Nothing -> error errorMsg

getMapInnards :: String -> InternalMap -> InternalMap
getMapInnards [] acc = acc
getMapInnards str acc =
    let 
        item = takeWhile (/= ',') str
        tuple = case (stripInfix "->" item) of
            Just (key, value) -> (key, value)
            Nothing -> error "Parser error."
        rest = drop (length item + 1) str
    in reverse $ getMapInnards rest (tuple : acc)

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
parseMaps (x:xs) acc =
    let
        striped = filter (/=' ') (stripElem x "Map" "Not a map.")
        parsed = getMapInnards striped []
    in parseMaps xs (parsed : acc)

parseScala :: String -> [InternalMap]
parseScala str =
    let
        listInnards = stripElem str "List" "Not a list."
        parsedData = parseMaps (parseList listInnards []) []
    in parsedData

fillTheGrid :: [InternalMap] -> [Marking] -> Grid
fillTheGrid [] grid = chunksOf n grid
fillTheGrid (x:xs) grid =
    let
        pos1 = read (findParam x "x" "x not defined.") :: Int
        pos2 = read (findParam x "y" "y not defined.") :: Int
        player = findParam x "v" "player not defined."
        index = n * pos1 + pos2
        newGrid = replaceNth index (Just (strToPlayer player)) grid
    in fillTheGrid xs newGrid

getWinSeqs :: Grid -> [[Marking]]
getWinSeqs grid = horizontal ++ vertical ++ [fDiag, bDiag]
  where horizontal = grid
        vertical = transpose grid
        fDiag = zipWith (!!) (reverse grid) [0..]
        bDiag = zipWith (!!) grid [0..]

winner :: String -> Maybe Char
winner map
    | winner' X  = Just 'x'
    | winner' O  = Just 'o'
    | otherwise = Nothing
    where
        grid = fillTheGrid (parseScala map) (concat emptyGrid)
        winner' :: Player -> Bool
        winner' player = any (all (== Just player)) $ getWinSeqs grid