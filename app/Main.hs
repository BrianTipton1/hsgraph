{-# LANGUAGE InstanceSigs #-}

module Main where

import GHC.Real (reduce)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitFailure, exitWith)
import System.FilePath.Posix (takeBaseName, takeDirectory)

-- Helpers
wrapStrInDoubleQuote :: String -> String
wrapStrInDoubleQuote s = "\"" ++ s ++ "\""
redifyString :: String -> String
redifyString s = "\ESC[31m" ++ s ++ "\ESC[0m"

-- End Helpers Section

-- Custom Types relating to the assignment
newtype Distance = Distance Integer
newtype Label = Label String
newtype Edge = Edge (Label, Distance) deriving (Show)

newtype NodeTable = NodeTable [NodeRow]

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

data NodeRow = NodeRow
    { node :: Node
    , distance :: Distance
    }

--- End custom types section

-- Subpar formatting for Pretty Printing of Assignment Types
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

-- Extra Credit Graphviz
data VizColor
    = VizBlue
    | VizRed
    | VizGreen
    | VizBlack

vizColorToString :: VizColor -> String
vizColorToString VizBlue = wrapStrInDoubleQuote "blue"
vizColorToString VizRed = wrapStrInDoubleQuote "red"
vizColorToString VizGreen = wrapStrInDoubleQuote "green"
vizColorToString VizBlack = wrapStrInDoubleQuote "green"

data VizEdgeType
    = VizEdgeSolid
    | VizEdgeDashed
    | VizEdgeBold
    | VizEdgeDotted

vizEdgeTypeToString :: VizEdgeType -> String
vizEdgeTypeToString VizEdgeSolid = wrapStrInDoubleQuote "solid"
vizEdgeTypeToString VizEdgeDashed = wrapStrInDoubleQuote "dashed"
vizEdgeTypeToString VizEdgeBold = wrapStrInDoubleQuote "bold"
vizEdgeTypeToString VizEdgeDotted = wrapStrInDoubleQuote "dotted"

data VizArrowType
    = VizArrowNormal
    | VizArrowCurve
    | VizArrowICurve
    | VizArrowVee

vizArrowTypeToString :: VizArrowType -> String
vizArrowTypeToString VizArrowNormal = wrapStrInDoubleQuote "normal"
vizArrowTypeToString VizArrowCurve = wrapStrInDoubleQuote "curve"
vizArrowTypeToString VizArrowICurve = wrapStrInDoubleQuote "icurve"
vizArrowTypeToString VizArrowVee = wrapStrInDoubleQuote "vee"

data VizEdge = VizEdge
    { vizSelf :: String
    , vizOther :: String
    , vizArrowType :: VizArrowType
    , vizArrow :: String
    , vizEdgeType :: VizEdgeType
    , vizDistance :: String
    , vizColor :: VizColor
    }

vizEdgeToStringBuilder :: VizEdge -> String
vizEdgeToStringBuilder
    VizEdge
        { vizArrowType = arrowType
        , vizEdgeType = edge
        , vizDistance = vizDistance
        , vizSelf = self
        , vizOther = other
        , vizColor = color
        , vizArrow = arrow
        } =
        "\n\tedge ["
            ++ "label=\""
            ++ vizDistance
            ++ "\", "
            ++ "color="
            ++ vizColorToString color
            ++ ", "
            ++ "arrowhead="
            ++ vizArrowTypeToString arrowType
            ++ "];\n\t"
            ++ show self
            ++ " "
            ++ arrow
            ++ " "
            ++ show other

data VizType = VizDiGraph | VizGraph

instance Show VizType where
    show :: VizType -> String
    show ta =
        vizTypeToString ta

vizTypeToString :: VizType -> String
vizTypeToString VizDiGraph = "digraph"
vizTypeToString VizGraph = "graph"
vizTypeToArrowString :: VizType -> String
vizTypeToArrowString VizDiGraph = "->"
vizTypeToArrowString VizGraph = "--"

data VizImage = VizImage
    { t :: VizType
    , edges :: [VizEdge]
    , name :: String
    }
instance Show VizImage where
    show :: VizImage -> String
    show VizImage{t = t, edges = edges, name = name} =
        vizTypeToString t ++ " " ++ name
            ++ "{ \n\t"
            ++ "size =\"8.5,11\";\n\t"
            ++ foldl (++) "\n\t" allEdges
            ++ "\n}"
      where
        allEdges = map vizEdgeToStringBuilder edges

writeGraphViz :: FilePath -> VizImage -> IO ()
writeGraphViz path image = writeFile path (show image)

-- End Grapviz Section

-- Command Line Section
helpScreen :: String -> String
helpScreen e =
    unlines
        [ "Usage: hsgraph [OPTIONS] FILE"
        , ""
        , "Description:"
        , "  hsgraph is a command-line utility for parsing and analyzing graphs. It operates on file(s) or multiple files within a directory. Optionally, it can also generate GraphViz graphs."
        , ""
        , "Options:"
        , "  -h, --help"
        , "    Show this help message and exit."
        , ""
        , "  -g, --graph"
        , "    Generate VizGraphs for the selected file(s)."
        , ""
        , "  -ng, --no-gif"
        , "    Do not generate GIFs from the image files. This option can only be used in conjunction with the -g/--graph option."
        , ""
        , "Arguments:"
        , "  FILE"
        , "    Required. The path to a file(s) and or a directory(s) to be parsed."
        , ""
        , "Examples:"
        , "  hsgraph myfile.txt"
        , "  hsgraph myfile.txt /path/to/directory"
        , "  hsgraph /path/to/directory"
        , "  hsgraph /path/to/directory -g"
        , "  hsgraph myfile.txt -g"
        , "  hsgraph /path/to/directory -g --no-gif"
        , "  hsgraph myfile.txt -g --no-gif"
        , ""
        , "Note:"
        , "  Ensure that the file or directory only contains files that hsgraph can parse."
        , "  The -ng/--no-gif option can only be used in conjunction with the -g/--graph option."
        , e
        ]

-- To print the help screen, you can use:
-- putStrLn helpScreen
data CommandLineOps
    = CmdHelp
    | CmdDirectoryPath FilePath
    | CmdGraph
    | CmdNoGif
    | CmdFilePath FilePath
    deriving (Eq, Show)

cmdHelpList :: [String]
cmdHelpList = ["-h", "--help"]
cmdGraphList :: [String]
cmdGraphList = ["-g", "--graph"]
cmdNoGifList :: [String]
cmdNoGifList = ["-ng", "--no-gif"]

strToOps :: String -> IO CommandLineOps
strToOps s
    | s `elem` cmdHelpList = return CmdHelp
    | s `elem` cmdGraphList = return CmdGraph
    | s `elem` cmdNoGifList = return CmdNoGif
    | otherwise = do
        pathExists <- doesFileExist s
        directory <- doesDirectoryExist s
        case (pathExists, directory) of
            (True, False) -> return $ CmdFilePath s
            (False, True) -> return $ CmdDirectoryPath s
            _ -> return CmdHelp

badOpCombinations :: [CommandLineOps] -> IO ()
badOpCombinations ops
    | containsHelp ops = do
        putStrLn $ helpScreen ""
        exitWith $ ExitFailure 1
    | isGifErr ops = do
        putStrLn $ helpScreen gifErrMsg
        exitWith $ ExitFailure 1
    | otherwise = return ()
  where
    containsHelp = elem CmdHelp
    isGifErr :: [CommandLineOps] -> Bool
    isGifErr xs = elem CmdNoGif xs && notElem CmdGraph xs
    gifErrMsg =
        redifyString $
            "\n  Error: must supply the "
                ++ wrapStrInDoubleQuote "-g/--graph"
                ++ " option to use "
                ++ wrapStrInDoubleQuote "-ng/--no-gif"

main :: IO ()
main = do
    args <- getArgs
    ops <- sequence $ strToOps <$> args
    -- graph <- pathToGraph $ head args
    badOpCombinations ops
