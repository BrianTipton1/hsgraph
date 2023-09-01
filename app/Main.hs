{-# LANGUAGE InstanceSigs #-}

module Main where

import System.Environment (getArgs)
import System.FilePath.Posix (takeBaseName, takeDirectory)

-- Custom Types
newtype Distance = Distance Integer
newtype Label = Label String
newtype Edge = Edge (Label, Distance) deriving (Show)
data Graph = Graph
    { nodes :: [Node]
    , nNodes :: Int
    , fileName :: String
    , directory :: FilePath
    }
    deriving (Show)

data Node = Node
    { label :: Label
    , adjacent :: [Edge]
    }

--- End custom types section

-- Subpar formatting for Pretty Printing
instance Show Label where
    show :: Label -> String
    show (Label lab) =
        "Node: " ++ lab

instance Show Distance where
    show :: Distance -> String
    show (Distance dis) =
        "Distance: " ++ show dis

instance Show Node where
    show :: Node -> String
    show (Node label adjacent) =
        "\n   {\n\tThis " ++ show label ++ "\n"
            ++ "\tAdjacent: "
            ++ show adjacent
            ++ "\n   }\n"

--- End formatting section

--- Parsing of the files to custom types
fileToLines :: FilePath -> IO [String]
fileToLines path = do
    fileContents <- readFile path
    let allLines = lines fileContents
    return $ tail allLines

getEdges :: [Edge] -> [String] -> [Edge]
getEdges acc list =
    if even $ length list
        then case list of
            (x : y : xs) ->
                getEdges (Edge (Label x, Distance $ read y :: Distance) : acc) xs
            _ -> acc
        else error "Incorrect dims when parsing edges"

lineToNode :: [Char] -> Node
lineToNode line =
    Node
        { label = Label node
        , adjacent = getEdges [] adjacent
        }
  where
    items = words line
    node = head items
    adjacent = tail items

linesToGraph :: FilePath -> IO [String] -> IO Graph
linesToGraph path ioxs = do
    xs <- ioxs
    return
        Graph
            { nodes = mapNodes xs
            , nNodes = length xs
            , directory = takeDirectory path
            , fileName = fileName
            }
  where
    mapNodes = map lineToNode
    fileName = takeBaseName path

pathToGraph :: FilePath -> IO Graph
pathToGraph path = linesToGraph path $ fileToLines path

--- End parsing section

main :: IO ()
main = do
    args <- getArgs
    graph <- pathToGraph $ head args
    print graph
