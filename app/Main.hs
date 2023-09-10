{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Exception (Exception, evaluate)
import Control.Monad (filterM)
import Data.Char (isAlpha, isDigit, toLower)
import Data.Either (fromRight, rights)
import Data.List (delete, find, foldl', minimumBy, partition)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import System.CPUTime (getCPUTime)
import System.Directory (createDirectory, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory)
import System.Environment (getArgs, getEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure, exitWith)
import System.FilePath.Posix (takeBaseName, takeDirectory, (</>))
import System.Process (readProcess, readProcessWithExitCode)
import Text.Printf (errorBadArgument)

-- Helpers
wrapStrInDoubleQuote :: String -> String
wrapStrInDoubleQuote s = "\"" ++ s ++ "\""

orangeifyString :: String -> String
orangeifyString str = "\ESC[48;2;255;165;0m" ++ str ++ "\ESC[0m"

redifyString :: String -> String
redifyString s = "\ESC[31m" ++ s ++ "\ESC[0m"

greenifyString :: String -> String
greenifyString str = "\x1b[42m" ++ str ++ "\x1b[0m"

blueifyString :: String -> String
blueifyString str = "\ESC[44m" ++ str ++ "\ESC[0m"

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

unwrapBothEither :: Either p p -> p
unwrapBothEither =
  \case
    Left a -> a
    Right a -> a

-- End Helpers Section

-- Command IO
data CommandErrors
  = UUIDNotFound String
  | DotNotFound String
  | ConvertNotFound String
  | PdfLatexNotFound String
  | WhoamiNotFound String

data Command
  = UUIDgen
  | Dot
  | Convert
  | PdfLatex
  | Whoami

instance Show Command where
  show :: Command -> String
  show = \case
    UUIDgen -> "uuidgen"
    Dot -> "dot"
    Convert -> "convert"
    PdfLatex -> "pdflatex"
    Whoami -> "whoami"

unwrapErr :: CommandErrors -> String
unwrapErr = \case
  UUIDNotFound e -> e
  DotNotFound e -> e
  ConvertNotFound e -> e
  PdfLatexNotFound e -> e
  WhoamiNotFound e -> e

commandToErr :: Command -> String -> CommandErrors
commandToErr =
  \case
    UUIDgen -> UUIDNotFound
    Dot -> DotNotFound
    Convert -> ConvertNotFound
    PdfLatex -> PdfLatexNotFound
    Whoami -> WhoamiNotFound

commandBuilder :: Command -> IO b -> IO b
commandBuilder cmd action = do
  maybeGenerate <- maybeCommandErr cmd
  case maybeGenerate of
    Nothing -> do
      putStrLn $ "Running command: " ++ show cmd
      action
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

convert :: FilePath -> [FilePath] -> IO ExitCode
convert outputDir files = commandBuilder Convert $ do
  putStrLn $ "Converting files: " ++ show files ++ " into a gif at directory: " ++ outputDir
  let outputFile = outputDir ++ "/result.gif"
  (ec, stdout, stderr) <-
    readProcessWithExitCode
      "convert"
      (["-delay", "100", "-loop", "0"] ++ files ++ [outputFile])
      ""
  putStrLn $ "STDOUT: " ++ stdout
  putStrLn $ "STDERR: " ++ stderr
  case ec of
    ExitSuccess -> putStrLn "Sucessfully compiled to gif" >> return ec
    _ -> putStrLn "Failed compiling to gif" >> return ec

newtype UUID = UUID String

instance Show UUID where
  show :: UUID -> String
  show (UUID uuid) = uuid

whoami :: IO String
whoami = commandBuilder UUIDgen $ do
  init <$> readProcess (show Whoami) [] ""

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
      , head (split '.' fileName ) ++ "." ++ fileType
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

generateLatexImage :: FilePath -> String -> String
generateLatexImage filePath caption =
  "\\begin{figure}[h]\n"
    ++ "  \\centering\n"
    ++ "  \\includegraphics[width=0.8\\textwidth]{"
    ++ filePath
    ++ "}\n"
    ++ "  \\caption{"
    ++ caption
    ++ "}\n"
    ++ "\\end{figure}"

generateLatexTable :: Table -> String
generateLatexTable (Table rows) =
  unlines
    ( [ "\\begin{table}[h]"
      , "  \\centering"
      , "  \\begin{tabular}{|c|c|}"
      , "    \\hline"
      , "    Node & Distance \\\\"
      , "    \\hline"
      ]
        ++ rowStrings
        ++ [ "    \\hline"
           , "  \\end{tabular}"
           , "  \\caption{Table built using Dijkstra's Algorithm}"
           , "\\end{table}"
           ]
    )
 where
  rowStrings = map rowToLatex rows
  rowToLatex (Row (Label lbl, Distance dist)) = "    " ++ lbl ++ " & " ++ show dist ++ " \\\\ \\hline"

generateLatexPreamble :: String -> String
generateLatexPreamble uname =
  unlines
    [ "\\documentclass{article}"
    , "\\usepackage{fancyhdr}"
    , "\\pagestyle{fancy}"
    , ""
    , "\\chead{Algorithms and Report generation implementation by Brian Tipton}"
    , "\\cfoot{Report/images compiled by " ++ uname ++ "}"
    , ""
    , "\\begin{document}"
    , ""
    , "\\begin{center}"
    , "\\large \\textbf {CS438 Search Algorithms} \\\\"
    , "\\bigskip"
    , ""
    , "\\small {Report comparing Dijkstra's Algorithm, Breadth First Search and Depth First Search}"
    , "\\bigskip"
    , ""
    , "\\large \\textit {\\today}"
    , "\\end{center}"
    , "\\bigskip"
    ]

mkSubsection :: Show a => a -> String
mkSubsection algo = "\\subsection{" ++ show algo ++ "}"

mkSection :: Show a => a -> String
mkSection fileName = "\\section{" ++ show fileName ++ "}"

latexEndDoc :: String
latexEndDoc = "\\end{document}"

algoSubSection :: Algorithm -> String
algoSubSection = mkSubsection

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
  pwd <- System.Environment.getEnv "PWD"
  fileExists <- System.Directory.doesFileExist (pwd </> results)
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
    System.Directory.createDirectoryIfMissing True (pwd </> results </> graphFileName </> child)

-- End File IO

--- Parsing of the files to custom types
getEdges :: [Edge] -> [String] -> [Edge]
getEdges acc list =
  if even $ length list
    then case list of
      (x : y : xs) ->
        getEdges (Edge{other = Label x, distanceToOther = Distance $ read y :: Distance} : acc) xs
      _ -> acc
    else errorBadArgument $ redifyString "Incorrect dims when parsing edges"

lineToNode :: String -> Node
lineToNode line =
  Node
    { identifier = Label node
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

instance Show VizColor where
  show :: VizColor -> String
  show = \case
    VizBlue -> "blue"
    VizRed -> "red"
    VizGreen -> "green"
    VizBlack -> "black"

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

newtype VizNodeProps where
  VizNodeProps ::
    {style :: String} ->
    VizNodeProps

instance Show VizNodeProps where
  show :: VizNodeProps -> String
  show
    VizNodeProps
      { style = style
      } =
      "[style=" ++ style ++ "];"

data VizNode where
  VizNode ::
    { vizId :: Label
    , properties :: VizNodeProps
    , edges :: [VizEdge]
    } ->
    VizNode

data VizEdge where
  VizEdge ::
    { vizOther :: Label
    , vizArrowType :: VizArrowType
    , vizArrow :: String
    , vizEdgeType :: VizEdgeType
    , vizDistance :: Distance
    , vizColor :: VizColor
    } ->
    VizEdge

data VizImage where
  VizImage ::
    { t :: VizType
    , vizNodes :: [VizNode]
    , name :: String
    , path :: FilePath
    } ->
    VizImage

instance Show VizImage where
  show :: VizImage -> String
  show
    VizImage
      { t = t
      , vizNodes = vizNodes
      , name = name
      , path = path
      } =
      show t ++ " " ++ name ++ "{\n\t"
        ++ nodeProps vizNodes
        ++ "\n\t"
        ++ vizNodesString vizNodes
        ++ "\n"
        ++ "}"
     where
      nodeProps nodes =
        foldl' (++) "" $ map (\VizNode{vizId = vizId, properties = properties} -> "\n\t" ++ label vizId ++ show properties) nodes
      vizNodesString :: [VizNode] -> String
      vizNodesString = foldl' (++) "" . map vizNodeToString
      vizNodeToString :: VizNode -> String
      vizNodeToString
        VizNode
          { vizId = vizId
          , properties = properties
          , edges = edges
          } = nodeEdge vizId edges
      nodeEdge :: Label -> [VizEdge] -> String
      nodeEdge parent edges =
        foldl' (++) "" $
          map
            ( \VizEdge
                { vizOther = vizOther
                , vizArrowType = vizArrowType
                , vizArrow = vizArrow
                , vizEdgeType = vizEdgeType
                , vizDistance = vizDistance
                , vizColor = vizColor
                } ->
                  "\n\t" ++ label parent ++ " " ++ vizArrow ++ " " ++ label vizOther
                    ++ "[label="
                    ++ wrapStrInDoubleQuote (show $ distance vizDistance)
                    ++ ", color="
                    ++ show vizColor
                    ++ "];"
            )
            edges

node2VizNode :: VizColor -> Bool -> Node -> VizNode
node2VizNode
  color
  show
  Node
    { identifier = identifier
    , adjacent = adjacent
    } =
    VizNode
      { vizId = identifier
      , edges = mapEdges adjacent
      , properties = VizNodeProps{style = if show then wrapStrInDoubleQuote "" else "invis"}
      }
   where
    mapEdges :: [Edge] -> [VizEdge]
    mapEdges =
      map
        ( \Edge
            { other = other
            , distanceToOther = distanceToOther
            } ->
              VizEdge
                { vizOther = other
                , vizArrowType = VizArrowNormal
                , vizArrow = "->"
                , vizEdgeType = VizEdgeSolid
                , vizDistance = distanceToOther
                , vizColor = color
                }
        )

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
  | s `elem` cmdStartNode startNode = return $ CmdStartNode startNode
  | otherwise = do
    pathExists <- System.Directory.doesFileExist s
    directory <- System.Directory.doesDirectoryExist s
    case (pathExists, directory) of
      (True, False) -> return $ CmdPath $ CmdFilePath s
      (False, True) -> return $ CmdPath $ CmdDirectoryPath s
      _ -> return CmdHelp
 where
  startNode = last $ split '=' s

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
          allContents <- System.Directory.listDirectory x
          files <- dirFilter allContents
          return $ acc ++ map (x </>) files
      )
        ()
    (x : xs) ->
      ( \_ -> do
          allContents <- System.Directory.listDirectory x
          files <- dirFilter allContents
          _allDirFiles (acc ++ map (x </>) files) xs
      )
        ()
 where
  isFileM :: FilePath -> IO Bool
  isFileM path = do
    result <- System.Directory.doesDirectoryExist path
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

_checkForNode :: Label -> Graph -> IO (Maybe Graph)
_checkForNode start graph = do
  if not (null bad)
    then return $ Just graph
    else do
      putStrLn $
        orangeifyString $
          "Warning: supplied starting node " ++ wrapStrInDoubleQuote (show start) ++ " not in graph at " ++ graphPath ++ " ..."
            ++ greenifyString
              ( "\nSkipping "
                  ++ graphPath
                  ++ " and continuing execution for now ..."
              )
      return Nothing
 where
  (bad, good) = partition (\x -> identifier x == start) (nodes graph)
  graphPath = wrapStrInDoubleQuote $ directory graph </> fileName graph

checkGraphs :: Label -> [Graph] -> IO [Graph]
checkGraphs label graphs = do
  graphs <- catMaybes <$> mapM (_checkForNode label) graphs
  if null graphs
    then do
      putStrLn $ redifyString "Error no suitable graphs supplied have the given starting node: " ++ blueifyString (show label)
      exitWith (ExitFailure 1)
    else return graphs

getGraphs :: [FilePath] -> Label -> IO [Graph]
getGraphs allFiles start = mapM pathToGraph (unique allFiles) >>= checkGraphs start

--- End Command Line Section

-----------------Course Work------------------

-- Custom Types relating to the assignment
data Algorithm = Dijkstras | BFS | DFS

instance Show Algorithm where
  show :: Algorithm -> String
  show =
    \case
      Dijkstras -> "Dijkstra's Algorithm"
      BFS -> "Breadth First Search"
      DFS -> "Depth First Search"

newtype Distance where
  Distance :: {distance :: Int} -> Distance
  deriving (Eq, Ord)

newtype Label where
  Label :: {label :: String} -> Label
  deriving (Eq, Ord)

data Edge where
  Edge :: {other :: Label, distanceToOther :: Distance} -> Edge
  deriving (Show)

data Graph = Graph
  { nodes :: [Node]
  , nNodes :: Int
  , fileName :: String
  , directory :: FilePath
  }
  deriving (Show)

data Node = Node
  { identifier :: Label
  , adjacent :: [Edge]
  }

--- End custom types section

-- Types Relating to recording the flow of the algorithms
newtype Row where
  Row :: {row :: (Label, Distance)} -> Row
  deriving (Show, Eq)

newtype Table where
  Table :: {rows :: [Row]} -> Table
  deriving (Show)

newtype From where
  From :: Label -> From
  deriving (Show)

newtype To where
  To :: Label -> To
  deriving (Show)

newtype Count where
  Count :: {count :: Int} -> Count
  deriving (Show)

type TableState = Table

data Transition where
  Transition ::
    { from :: From
    , to :: To
    , transitionDistance :: Distance
    , transitionCount :: Count
    , state :: TableState
    } ->
    Transition
  deriving (Show)

type TransitionSpace = [Either Transition Transition]

tableToTuples :: Table -> [(Label, Distance)]
tableToTuples t = map row $ rows t

eitherTransitionCount :: Either Transition Transition -> Count
eitherTransitionCount t = count
 where
  count = transitionCount $ unwrapBothEither t

-- End Recording Types

-- Dijkstra Specific
dijkstra :: Graph -> Label -> (Table, TransitionSpace)
dijkstra graph start =
  let initial = Table [Row (label, if label == start then Distance 0 else Distance maxBound) | Node label _ <- nodes graph]
   in dijkstra' graph [] (initial, [])
 where
  dijkstra' :: Graph -> [Label] -> (Table, TransitionSpace) -> (Table, TransitionSpace)
  dijkstra' _ visited (table, space)
    | all ((`elem` visited) . fst) (tableToTuples table) = (table, space)
  dijkstra' graph visited (table, space) =
    let unvisited = filter (\(l, _) -> l `notElem` visited) (tableToTuples table)
        (nextLabel, nextDistance) = minimumBy (comparing snd) unvisited
        Just (Node _ adjacents) = find (\(Node label _) -> label == nextLabel) (nodes graph)
        updatedDistances = foldl' (updateDistance nextLabel nextDistance) (table, space) adjacents
     in dijkstra' graph (nextLabel : visited) updatedDistances

updateDistance :: Label -> Distance -> (Table, TransitionSpace) -> Edge -> (Table, TransitionSpace)
updateDistance current (Distance currentDistance) (distanceTable, space) Edge{other = destination, distanceToOther = Distance weight}
  | newDistance < destinationDistance =
    ( Table $ Row (destination, Distance newDistance) : delete (Row (destination, Distance destinationDistance)) (rows distanceTable)
    , Right
        Transition
          { from = From current
          , to = To destination
          , transitionDistance = Distance newDistance
          , transitionCount = currCount
          , state = distanceTable
          } :
      space
    )
  | otherwise =
    ( distanceTable
    , Left
        Transition
          { from = From current
          , to = To destination
          , transitionDistance = Distance destinationDistance
          , transitionCount = currCount
          , state = distanceTable
          } :
      space
    )
 where
  Just (Distance destinationDistance) = lookup destination (tableToTuples distanceTable)
  newDistance = currentDistance + weight
  currCount =
    if null space
      then Count 1
      else Count $ count (eitherTransitionCount (head space)) + 1

-- End Dijkstra Stuff


-------------- End Course Work Section ---------------
transitionsToImages :: Graph -> TransitionSpace -> [VizImage]
transitionsToImages graph transition =
  transitionsToImages' graph transition []
 where
  transitionsToImages' :: Graph -> TransitionSpace -> [VizImage] -> [VizImage]
  transitionsToImages' _ [] acc = acc
  transitionsToImages' graph@Graph{nodes = nodes, fileName = fileName, directory = directory} (transition : rest) acc =
    transitionsToImages' graph rest (fixImage start transition : acc)
   where
    start =
      if null acc
        then
          VizImage
            { t = VizDiGraph
            , vizNodes = map (node2VizNode VizBlack True) nodes
            , name = fileName
            , path = directory
            }
        else head acc
    fixImage :: VizImage -> Either Transition Transition -> VizImage
    fixImage img = \case
      Left t -> fixFromTransition t VizRed img
      Right t -> fixFromTransition t VizGreen img
    fixFromTransition
      Transition
        { from = from
        , to = to
        , transitionDistance = transitionDistance
        , transitionCount = transitionCount
        , state = state
        }
      color
      img = replaceVizColor color img from to
    replaceVizColor :: VizColor -> VizImage -> From -> To -> VizImage
    replaceVizColor newColor image@VizImage{vizNodes = nodes} (From from) (To to) =
      let newNodes = map (replaceEdgeColor from to) nodes
       in image{vizNodes = newNodes}
     where
      replaceEdgeColor parent child node@VizNode{vizId = nodeId, edges = es}
        | nodeId == parent = node{edges = map (\e -> if vizOther e == child then e{vizColor = newColor} else e) es}
        | otherwise = node

compileAllImages :: [VizImage] -> IO ()
compileAllImages images = do
  pwd <- getCurrentDirectory
  pngFiles <- sequence [doDotAfterWrite (index + 1) img pwd | (index, img) <- zip [0 ..] images]
  exitCode <- convert (concat [pwd, "/results/", name $ head images]) pngFiles
  putStrLn $ "GIF compilation returned: " ++ show exitCode
 where
  writeSingleImage :: Int -> VizImage -> FilePath -> IO ()
  writeSingleImage index img@VizImage{name = name} pwd = do
    let dirPath = concat [pwd, "/results/", name, "/"]
    createDirectoryIfMissing True dirPath
    let filePath = concat [dirPath, name, show index, ".dot"]
    writeFile filePath (show img)
    putStrLn $ "Written file: " ++ filePath

  doDotAfterWrite :: Int -> VizImage -> FilePath -> IO FilePath
  doDotAfterWrite index img pwd = do
    let dotFileName = concat [pwd, "/results/", name img, "/", name img, show index, ".dot"]
    let pngFileName = concat [pwd, "/results/", name img, "/", name img, show index, ".png"]
    writeSingleImage index img pwd
    _ <- dot dotFileName "png"
    return pngFileName

main :: IO ()
main = do
  args <- getArgs
  ops <- sequence (strToOps <$> args) >>= checkOps
  let startLabel = getStartingLabel ops
  let dirs = mapCmdPathToPath (filter isCmdDirectory ops)
  readDirFiles <- allDirFiles dirs
  let allFiles = mapCmdPathToPath (filter isCmdFile ops) ++ readDirFiles
  let resultPaths = map takeBaseName allFiles
  graphs <- getGraphs allFiles startLabel
  ((fin, res), time) <- timeF $ dijkstra (head graphs) startLabel
  let headGraphs = head graphs
  let imgs = reverse $ transitionsToImages headGraphs (reverse res)
  mapM_ createOutDirStructure resultPaths >> compileAllImages imgs
  print "Done"
