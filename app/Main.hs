module Main where

type Distance = Integer
type Edge = (String, Distance)
type Graph = [Node]

data Node = Node
    { adjacent :: [Edge]
    , label :: String
    }
    deriving (Show)

fileToLines :: FilePath -> IO [String]
fileToLines path = do
    fileContents <- readFile path
    let allLines = lines fileContents
    return $ tail (init allLines)

getEdges :: [(String, Distance)] -> [String] -> [Edge]
getEdges acc list =
    if even $ length list
        then case list of
            (x : y : xs) ->
                getEdges ((x, read y :: Distance) : acc) xs
            _ -> acc
        else error "Incorrect dims when parsing edges"

lineToNode :: [Char] -> Node
lineToNode line =
    Node
        { label = node
        , adjacent = getEdges [] adjacent
        }
  where
    items = words line
    node = head items
    adjacent = tail items

linesToGraph :: [String] -> Graph
linesToGraph = map lineToNode

main :: IO ()
main = do
    lines <-
        fileToLines "/Users/brian/Documents/School/CS438/CS438SearchAlgorithmsHW1/resources/data/graphPosMidA"
    print $ linesToGraph lines
