{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Exception (Exception, evaluate)
import Control.Monad (filterM)
import Data.Char (isAlpha, isDigit, toLower)
import Data.Either (fromRight, rights)
import Data.List (delete, find, foldl', minimumBy, partition)
import Data.Ord (comparing)
import System.CPUTime (getCPUTime)
import System.Directory (createDirectory, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getArgs, getEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure, exitWith)
import System.FilePath.Posix (takeBaseName, takeDirectory, (</>))
import System.Process (readProcess, readProcessWithExitCode)
import Text.Printf (errorBadArgument)

-- Helpers
wrapStrInDoubleQuote :: String -> String
wrapStrInDoubleQuote s = "\"" ++ s ++ "\""
redifyString :: String -> String
redifyString s = "\ESC[31m" ++ s ++ "\ESC[0m"

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x : xs) = x : unique (filter (/= x) xs)

split :: Char -> String -> [String]
split _ "" = [""]
split delimiter (x : xs)
    | x == delimiter = "" : rest
    | otherwise = (x : head rest) : tail rest
  where
    rest = split delimiter xs

_timer :: IO a -> IO (a, String)
_timer f = do
    startTime <- getCPUTime
    result <- f
    endTime <- getCPUTime
    let diffPico = fromIntegral (endTime - startTime)
    let timeStr
            | diffPico < 1000 = "$$" ++ show diffPico ++ " ps$$"
            | diffPico < 10 ^ 6 = "$$" ++ show (diffPico / 10 ^ 3) ++ " ns$$"
            | diffPico < 10 ^ 9 = "$$" ++ show (diffPico / 10 ^ 6) ++ " \\mu s$$"
            | otherwise = "$$" ++ show (diffPico / 10 ^ 9) ++ " ms$$"
    return (result, timeStr)

timeF :: a -> IO (a, String)
timeF f = _timer (evaluate f)

-- End Helpers Section

-- Command IO
data CommandErrors
    = UUIDNotFound String
    | DotNotFound String
    | ConvertNotFound String
    | PdfLatexNotFound String

data Command
    = UUIDgen
    | Dot
    | Convert
    | PdfLatex

instance Show Command where
    show :: Command -> String
    show UUIDgen = "uuidgen"
    show Dot = "dot"
    show Convert = "convert"
    show PdfLatex = "pdflatex"

unwrapErr :: CommandErrors -> String
unwrapErr (UUIDNotFound e) = e
unwrapErr (DotNotFound e) = e
unwrapErr (ConvertNotFound e) = e
unwrapErr (PdfLatexNotFound e) = e

commandToErr :: Command -> String -> CommandErrors
commandToErr =
    \case
        UUIDgen -> UUIDNotFound
        Dot -> DotNotFound
        Convert -> ConvertNotFound
        PdfLatex -> PdfLatexNotFound

commandBuilder :: Command -> IO b -> IO b
commandBuilder cmd action = do
    maybeGenerate <- maybeCommandErr cmd
    case maybeGenerate of
        Nothing -> action
        Just e -> error $ unwrapErr e

data LatexOutFormat = Pdf | Png
instance Show LatexOutFormat where
    show :: LatexOutFormat -> String
    show Pdf = "pdf"
    show Png = "png"

pdfLatex :: FilePath -> FilePath -> LatexOutFormat -> IO ExitCode
pdfLatex texPath outDir fmt = commandBuilder PdfLatex $ do
    (ec, _, _) <-
        readProcessWithExitCode
            (show PdfLatex)
            ["-output-directory=" ++ outDir, "-output-format=" ++ show fmt]
            ""
    return ec

newtype UUID = UUID String
instance Show UUID where
    show :: UUID -> String
    show (UUID uuid) = uuid

uuidgen :: IO UUID
uuidgen = commandBuilder UUIDgen $ do
    uuid <- init <$> readProcess (show UUIDgen) [] ""
    let removedHyphens = filter (/= '-') uuid
    let (alpha, numeric) = partition isAlpha removedHyphens
    return $ UUID $ alpha ++ numeric

dot :: String -> String -> IO ExitCode
dot fileName fileType = commandBuilder Dot $ do
    (ec, _, _) <-
        readProcessWithExitCode
            (show Dot)
            [ "-T" ++ fileType
            , "-Gdpi=300"
            , fileName
            , "-o"
            , fileName ++ "." ++ fileType
            ]
            ""
    return ec

maybeCommandErr :: Command -> IO (Maybe CommandErrors)
maybeCommandErr cmd = do
    let strCmd = show cmd
    (ec, _, _) <- readProcessWithExitCode "which" [strCmd] ""
    if ec == ExitSuccess
        then return Nothing
        else do
            let err = commandToErr cmd
            return $ Just (err $ redifyString $ "Error command " ++ wrapStrInDoubleQuote strCmd ++ " not found ...")

-- End Command IO Section

-- Report/Latex Stuff

generateLatexTable :: DijkstraTable -> Label -> String -> String
generateLatexTable (DijkstraTable rows) (Label startLabel) filename =
    unlines
        ( [ "\\begin{table}[h]"
          , "  \\centering"
          , "  \\caption{Distances from Node " ++ startLabel ++ " to all other nodes using Dijkstra's Algorithm from the file " ++ filename ++ "}"
          , "  \\begin{tabular}{|c|c|}"
          , "    \\hline"
          , "    Node & Distance \\\\"
          , "    \\hline"
          ]
            ++ rowStrings
            ++ [ "    \\hline"
               , "  \\end{tabular}"
               , "\\end{table}"
               ]
        )
  where
    rowStrings = map rowToLatex rows
    rowToLatex (DijkstraRow (Label lbl, Distance dist)) = "    " ++ lbl ++ " & " ++ show dist ++ " \\\\ \\hline"

---

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

-- File IO
fileToLines :: FilePath -> IO [String]
fileToLines path = do
    fileContents <- readFile path
    let allLines = lines fileContents
    return $ tail allLines

createOutDirStructure :: String -> IO ()
createOutDirStructure graphFileName = do
    pwd <- getEnv "PWD"
    fileExists <- doesFileExist (pwd </> results)
    if fileExists
        then error $ redifyString "A regular file exists at $PWD/results please move this to continue ..."
        else
            mapM_
                (dirStructureBuilder pwd)
                [ "intermediteImages"
                , "intermediateGraphVizFiles"
                , "report"
                , "finalImages"
                ]
  where
    results = "results"
    dirStructureBuilder pwd child =
        createDirectoryIfMissing True (pwd </> results </> graphFileName </> child)

-- End File IO

--- Parsing of the files to custom types
getEdges :: [Edge] -> [String] -> [Edge]
getEdges acc list =
    if even $ length list
        then case list of
            (x : y : xs) ->
                getEdges (Edge (Label x, Distance $ read y :: Distance) : acc) xs
            _ -> acc
        else errorBadArgument $ redifyString "Incorrect dims when parsing edges"

lineToNode :: String -> Node
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

-- Extra Credit GraphViz
data VizColor
    = VizBlue
    | VizRed
    | VizGreen
    | VizBlack
    deriving (Eq)

vizColorToString :: VizColor -> String
vizColorToString VizBlue = wrapStrInDoubleQuote "blue"
vizColorToString VizRed = wrapStrInDoubleQuote "red"
vizColorToString VizGreen = wrapStrInDoubleQuote "green"
vizColorToString VizBlack = wrapStrInDoubleQuote "black"

data VizEdgeType
    = VizEdgeSolid
    | VizEdgeDashed
    | VizEdgeBold
    | VizEdgeDotted
    deriving (Eq)

instance Show VizEdgeType where
    show :: VizEdgeType -> String
    show e =
        vizEdgeTypeToString e

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
    deriving (Eq)

instance Show VizArrowType where
    show :: VizArrowType -> String
    show e =
        vizArrowTypeToString e

data VizNodeProps = VizNodeProps
    { width :: Int
    , height :: Int
    , style :: String
    , shape :: String
    }
    deriving (Eq)

instance Show VizNodeProps where
    show :: VizNodeProps -> String
    show
        VizNodeProps
            { width = width
            , height = height
            , style = style
            , shape = shape
            } =
            "[style=" ++ style
                ++ ", shape="
                ++ shape
                ++ ", width="
                ++ show width
                ++ ", height="
                ++ show height
                ++ "];"

data VizNode = VizNode
    { vizId :: String
    , nodeProps :: VizNodeProps
    }
    deriving (Eq)

data VizEdge = VizEdge
    { vizSelf :: VizNode
    , vizOther :: VizNode
    , vizArrowType :: VizArrowType
    , vizArrow :: String
    , vizEdgeType :: VizEdgeType
    , vizDistance :: String
    , vizColor :: VizColor
    }
    deriving (Eq)

instance Show VizEdge where
    show :: VizEdge -> String
    show VizEdge{vizArrowType = vizArrowType, vizColor = vizColor, vizArrow = vizArrow, vizDistance = distance, vizSelf = VizNode{vizId = selfId}, vizOther = VizNode{vizId = otherId}} =
        selfId ++ " " ++ vizArrow ++ " " ++ otherId
            ++ "[label="
            ++ wrapStrInDoubleQuote distance
            ++ ", arrowhead="
            ++ show vizArrowType
            ++ ", color="
            ++ vizColorToString vizColor
            ++ ", style="
            ++ show vizArrowType
            ++ "]"

data VizImage = VizImage
    { t :: VizType
    , edges :: [VizEdge]
    , name :: String
    , path :: FilePath
    }

instance Show VizImage where
    show :: VizImage -> String
    show VizImage{t = t, edges = edges, name = name} =
        preamble ++ nodePropertyDefs ++ edgeStrs ++ "\n}"
      where
        preamble = show t ++ " " ++ name ++ " {"
        nodePropertyDefs =
            concatNlTab $
                map
                    (\VizNode{vizId = vizId, nodeProps = props} -> vizId ++ " " ++ show props ++ "\t")
                    (edgesToUniqueNodes edges)
        edgeStrs = concatNlTab $ map show edges
        concatNlTab = foldl' (\x y -> x ++ "\n\t" ++ y) ""

edgesToAllNodes :: [VizEdge] -> [VizNode]
edgesToAllNodes =
    concatMap
        (\VizEdge{vizSelf = vizSelf, vizOther = vizOther} -> [vizSelf, vizOther])

edgesToUniqueNodes :: [VizEdge] -> [VizNode]
edgesToUniqueNodes = unique . edgesToAllNodes

data VizType = VizDiGraph | VizGraph

vizArrowTypeToString :: VizArrowType -> String
vizArrowTypeToString VizArrowNormal = wrapStrInDoubleQuote "normal"
vizArrowTypeToString VizArrowCurve = wrapStrInDoubleQuote "curve"
vizArrowTypeToString VizArrowICurve = wrapStrInDoubleQuote "icurve"
vizArrowTypeToString VizArrowVee = wrapStrInDoubleQuote "vee"

instance Show VizType where
    show :: VizType -> String
    show ta =
        case ta of
            VizDiGraph -> "digraph"
            VizGraph -> "graph"

vizTypeArrow :: VizType -> String
vizTypeArrow VizDiGraph = "->"
vizTypeArrow VizGraph = "--"

writeGraphViz :: VizImage -> IO ()
writeGraphViz image =
    writeFile (path image) (show image)

-- End Grapviz Section

-- Command Line Section
helpScreen :: String -> String
helpScreen e =
    unlines
        [ "Usage: hsgraph [OPTIONS] (FILE|DIR)*"
        , ""
        , "Description:"
        , "    hsgraph is a command-line utility for parsing and analyzing graphs using Dijkstra's Algorithm, BFS and DFS. It operates on file(s) or multiple files within a directory(s). Optionally, it can also generate GraphViz graphs. The report and images are generated to $PWD/results"
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
        , "  -s=2, --start-node=2"
        , "    Optionally supply a node to start algorithms from"
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
        , "  Ensure that the file(s) or directory(s) supplied only contains files that hsgraph can parse."
        , "  The -ng/--no-gif option can only be used in conjunction with the -g/--graph option."
        , e
        ]

data CmdPath
    = CmdDirectoryPath {getFilePath :: FilePath}
    | CmdFilePath {getFilePath :: FilePath}
    deriving (Eq, Show)

data CommandLineOption
    = CmdPath CmdPath
    | CmdHelp
    | CmdGraph
    | CmdNoGif
    | CmdStartNode String
    deriving (Eq, Show)

mapCmdPathToPath :: [CommandLineOption] -> [FilePath]
mapCmdPathToPath =
    map
        ( ( \case
                CmdDirectoryPath p -> p
                CmdFilePath p -> p
          )
            . (\(CmdPath fp) -> fp)
        )

cmdHelpList :: [String]
cmdHelpList = ["-h", "--help"]
cmdGraphList :: [String]
cmdGraphList = ["-g", "--graph"]
cmdNoGifList :: [String]
cmdNoGifList = ["-ng", "--no-gif"]
cmdStartNode :: String -> [String]
cmdStartNode s = (++ s) <$> ["-s=", "--start-node="]

strToOps :: String -> IO CommandLineOption
strToOps s
    | s `elem` cmdHelpList = return CmdHelp
    | s `elem` cmdGraphList = return CmdGraph
    | s `elem` cmdNoGifList = return CmdNoGif
    | s `elem` cmdStartNode (last startNode) = return $ CmdStartNode (last startNode)
    | otherwise = do
        pathExists <- doesFileExist s
        directory <- doesDirectoryExist s
        case (pathExists, directory) of
            (True, False) -> return $ CmdPath $ CmdFilePath s
            (False, True) -> return $ CmdPath $ CmdDirectoryPath s
            _ -> return CmdHelp
  where
    startNode = split '=' s

checkOps :: [CommandLineOption] -> IO [CommandLineOption]
checkOps ops
    | containsHelp ops || null ops = do
        putStrLn $ helpScreen ""
        exitWith $ ExitFailure 1
    | isGifErr ops = do
        putStrLn $ helpScreen gifErrMsg
        exitWith $ ExitFailure 1
    | otherwise = return ops
  where
    containsHelp = elem CmdHelp
    isGifErr :: [CommandLineOption] -> Bool
    isGifErr xs = elem CmdNoGif xs && notElem CmdGraph xs
    gifErrMsg =
        redifyString $
            "\n  Error: must supply the "
                ++ wrapStrInDoubleQuote "-g/--graph"
                ++ " option to use "
                ++ wrapStrInDoubleQuote "-ng/--no-gif"

_allDirFiles :: [FilePath] -> [FilePath] -> IO [FilePath]
_allDirFiles acc dirs =
    case dirs of
        [] -> return acc
        [x] ->
            ( \_ -> do
                allContents <- listDirectory x
                files <- dirFilter allContents
                return $ acc ++ map (x </>) files
            )
                ()
        (x : xs) ->
            ( \_ -> do
                allContents <- listDirectory x
                files <- dirFilter allContents
                _allDirFiles (acc ++ map (x </>) files) xs
            )
                ()
  where
    isFileM :: FilePath -> IO Bool
    isFileM path = do
        result <- doesDirectoryExist path
        return (not result)
    dirFilter :: [FilePath] -> IO [FilePath]
    dirFilter = filterM isFileM

allDirFiles :: [FilePath] -> IO [FilePath]
allDirFiles = _allDirFiles []

isCmdDirectory :: CommandLineOption -> Bool
isCmdDirectory x =
    case x of
        (CmdPath (CmdDirectoryPath x)) -> True
        _ -> False

isCmdFile :: CommandLineOption -> Bool
isCmdFile x =
    case x of
        (CmdPath (CmdFilePath x)) -> True
        _ -> False

getStartingLabel :: [CommandLineOption] -> Label
getStartingLabel cmds =
    case length startNodes of
        0 -> Label "1"
        1 -> Label $ head startNodes
        _ -> error $ redifyString "Only one start node can be specified ..."
  where
    startNodes = [i | CmdStartNode i <- cmds]

--- End Command Line Section

-----------------Course Work------------------

-- Custom Types relating to the assignment
newtype Distance
    = Distance Int
    deriving (Eq, Ord)
newtype Label
    = Label String
    deriving (Eq, Ord)
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

-- Dijkstra Specific
newtype DijkstraRow = DijkstraRow (Label, Distance) deriving (Show, Eq)
newtype DijkstraTable = DijkstraTable [DijkstraRow] deriving (Show)

newtype From = From Label deriving (Show)
newtype To = To Label deriving (Show)
type TableState = DijkstraTable
newtype Transition = Transition ((From, To), TableState) deriving (Show)
type TransitionSpace = [Either Transition Transition]

rowToTuple :: DijkstraRow -> (Label, Distance)
rowToTuple (DijkstraRow (l, d)) = (l, d)

unwrapRows :: DijkstraTable -> [DijkstraRow]
unwrapRows (DijkstraTable rows) = rows

tableToTuples :: DijkstraTable -> [(Label, Distance)]
tableToTuples t = map rowToTuple $ unwrapRows t

dijkstra :: Graph -> Label -> (DijkstraTable, TransitionSpace)
dijkstra graph start =
    let initial = DijkstraTable [DijkstraRow (label, if label == start then Distance 0 else Distance maxBound) | Node label _ <- nodes graph]
     in dijkstra' graph [] (initial, [])

dijkstra' :: Graph -> [Label] -> (DijkstraTable, TransitionSpace) -> (DijkstraTable, TransitionSpace)
dijkstra' _ visited (table, space)
    | all ((`elem` visited) . fst) (tableToTuples table) = (table, space)

dijkstra' graph visited (table, space) =
    let unvisited = filter (\(l, _) -> l `notElem` visited) (tableToTuples table)
        (nextLabel, nextDistance) = minimumBy (comparing snd) unvisited
        Just (Node _ adjacents) = find (\(Node label _) -> label == nextLabel) (nodes graph)
        updatedDistances = foldl' (updateDistance nextLabel nextDistance) (table, space) adjacents
     in dijkstra' graph (nextLabel : visited) updatedDistances

updateDistance :: Label -> Distance -> (DijkstraTable, TransitionSpace) -> Edge -> (DijkstraTable, TransitionSpace)
updateDistance current (Distance currentDistance) (distanceTable, space) (Edge (destination, Distance weight))
    | newDistance < destinationDistance =
        ( DijkstraTable $ DijkstraRow (destination, Distance newDistance) : delete (DijkstraRow (destination, Distance destinationDistance)) (unwrapRows distanceTable)
        , Right (Transition ((From current, To destination), distanceTable)) : space
        )
    | otherwise =
        ( distanceTable
        , Left (Transition ((From current, To destination), distanceTable)) : space
        )
  where
    Just (Distance destinationDistance) = lookup destination (tableToTuples distanceTable)
    newDistance = currentDistance + weight

-- End Dijkstra Stuff

-------------- End Course Work Section ---------------

main :: IO ()
main = do
    args <- getArgs
    ops <- sequence (strToOps <$> args) >>= checkOps
    let startLabel = getStartingLabel ops
    let dirs = mapCmdPathToPath (filter isCmdDirectory ops)
    readDirFiles <- allDirFiles dirs
    let allFiles = mapCmdPathToPath (filter isCmdFile ops) ++ readDirFiles
    graphs <- mapM pathToGraph $ unique allFiles
    ((fin, res), time) <- timeF $ dijkstra (head graphs) startLabel
    -- putStrLn $ generateLatexTable fin startLabel (fileName $ head graphs)
    print fin
