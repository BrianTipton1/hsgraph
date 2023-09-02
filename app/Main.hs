{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Exception (Exception)
import Control.Monad (filterM)
import Data.Char (isAlpha, isDigit, toLower)
import Data.List (foldl', partition)
import System.Directory (createDirectory, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getArgs, getEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure, exitWith)
import System.FilePath.Posix (takeBaseName, takeDirectory, (</>))
import System.Process (readCreateProcessWithExitCode, readProcess, readProcessWithExitCode)
import Text.Printf (errorBadArgument)

-- Helpers
wrapStrInDoubleQuote :: String -> String
wrapStrInDoubleQuote s = "\"" ++ s ++ "\""
redifyString :: String -> String
redifyString s = "\ESC[31m" ++ s ++ "\ESC[0m"

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x : xs) = x : unique (filter (/= x) xs)

-- End Helpers Section

-- Command IO
data CommandErrors
    = UUIDNotFound String
    | DotNotFound String
    | ConvertNotFound String

data Command
    = UUIDgen
    | Dot
    | Convert

instance Show Command where
    show :: Command -> String
    show UUIDgen = "uuidgen"
    show Dot = "dot"
    show Convert = "convert"

unwrapErr :: CommandErrors -> String
unwrapErr (UUIDNotFound e) = e
unwrapErr (DotNotFound e) = e
unwrapErr (ConvertNotFound e) = e

commandToErr :: Command -> String -> CommandErrors
commandToErr =
    \case
        UUIDgen -> UUIDNotFound
        Dot -> DotNotFound
        Convert -> ConvertNotFound

commandBuilder :: Command -> IO b -> IO b
commandBuilder cmd action = do
    maybeGenerate <- maybeCommandErr cmd
    case maybeGenerate of
        Nothing -> action
        Just e -> error $ unwrapErr e

uuidgen :: IO String
uuidgen = commandBuilder UUIDgen $ do
    uuid <- init <$> readProcess (show UUIDgen) [] ""
    let removedHyphens = filter (/= '-') uuid
    let (alpha, numeric) = partition isAlpha removedHyphens
    return $ alpha ++ numeric

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

-- Custom Types relating to the assignment
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
                , "reports"
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
                    (\VizNode{vizId = vizId, nodeProps = props} -> vizId ++ " " ++ show props ++ "\n\t")
                    (edgesToUniqueNodes edges)
        edgeStrs = concatNlTab $ map (\x -> show x ++ "\n\t") edges
        concatNlTab = foldl' (++) "\n\t"

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

unDirGraphToVizImage :: FilePath -> Graph -> VizImage
unDirGraphToVizImage
    dir
    Graph
        { fileName = fileName
        , nodes = nodes
        } =
        VizImage
            { t = VizGraph
            , edges = unique $ mapNodesToVizEdges nodes
            , name = fileName
            , path = dir </> fileName
            }

mapNodesToVizEdges :: [Node] -> [VizEdge]
mapNodesToVizEdges =
    map
        ( \Node
            { label = label
            , adjacent = adjacent
            } ->
                VizEdge
                    {
                    }
        )

-- End Grapviz Section

-- Command Line Section
helpScreen :: String -> String
helpScreen e =
    unlines
        [ "Usage: hsgraph [OPTIONS] (FILE|DIR)*"
        , ""
        , "Description:"
        , "  hsgraph is a command-line utility for parsing and analyzing graphs. It operates on file(s) or multiple files within a directory(s). Optionally, it can also generate GraphViz graphs."
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

strToOps :: String -> IO CommandLineOption
strToOps s
    | s `elem` cmdHelpList = return CmdHelp
    | s `elem` cmdGraphList = return CmdGraph
    | s `elem` cmdNoGifList = return CmdNoGif
    | otherwise = do
        pathExists <- doesFileExist s
        directory <- doesDirectoryExist s
        case (pathExists, directory) of
            (True, False) -> return $ CmdPath $ CmdFilePath s
            (False, True) -> return $ CmdPath $ CmdDirectoryPath s
            _ -> return CmdHelp

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

--- End Command Line Section

main :: IO ()
main = do
    args <- getArgs
    ops <- sequence (strToOps <$> args) >>= checkOps
    let dirs = mapCmdPathToPath (filter isCmdDirectory ops)
    readDirFiles <- allDirFiles dirs
    let allFiles = mapCmdPathToPath (filter isCmdFile ops) ++ readDirFiles
    print $ unique allFiles
