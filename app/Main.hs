{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Exception (Exception, evaluate)
import Control.Monad (filterM, when)
import Data.Char (isAlpha, isDigit, toLower)
import Data.Either (fromRight, rights)
import Data.List (delete, find, foldl', minimumBy, partition, sortBy, sortOn)
import Data.Map (Map, fromList, toList)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import System.CPUTime (getCPUTime)
import System.Directory (createDirectory, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory)
import System.Environment (getArgs, getEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure, exitWith)
import System.FilePath.Posix (takeBaseName, takeDirectory, (</>))
import System.Process (readProcess, readProcessWithExitCode)
import Text.Printf (errorBadArgument, printf)

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

unwrapBothEither :: Either p p -> p
unwrapBothEither =
  \case
    Left a -> a
    Right a -> a

countRights :: [Either a b] -> Int
countRights = length . filter isRight
 where
  isRight (Right _) = True
  isRight _ = False

-- End Helpers Section

-- Command IO
data CommandErrors
  = DotNotFound String
  | ConvertNotFound String
  | PdfLatexNotFound String
  | WhoamiNotFound String

data Command
  = Dot
  | Convert
  | PdfLatex
  | Whoami

instance Show Command where
  show :: Command -> String
  show = \case
    Dot -> "dot"
    Convert -> "convert"
    PdfLatex -> "pdflatex"
    Whoami -> "whoami"

unwrapErr :: CommandErrors -> String
unwrapErr = \case
  DotNotFound e -> e
  ConvertNotFound e -> e
  PdfLatexNotFound e -> e
  WhoamiNotFound e -> e

commandToErr :: Command -> String -> CommandErrors
commandToErr =
  \case
    Dot -> DotNotFound
    Convert -> ConvertNotFound
    PdfLatex -> PdfLatexNotFound
    Whoami -> WhoamiNotFound

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
      ["-output-format=" ++ show fmt, "-output-directory=" ++ outDir, show texPath]
      ""
  return ec

convert :: FilePath -> [FilePath] -> IO ExitCode
convert outputDir files = commandBuilder Convert $ do
  putStrLn $ "Attempting conversion of the following image files to gif: " ++ foldl' (\acc x -> acc ++ "\n" ++ x) "" files ++ "\nResult gif being saved at: " ++ finalDir
  let outputFile = finalDir </> "result.gif"
  (ec, stdout, stderr) <-
    readProcessWithExitCode
      "convert"
      (["-delay", "100", "-loop", "0"] ++ files ++ [outputFile])
      ""
  case ec of
    ExitSuccess -> putStrLn "Sucessfully converted to gif" >> return ec
    _ -> putStrLn (redifyString "Failed conversion to gif") >> return ec
 where
  finalDir = outputDir </> "finalImages"

whoami :: IO String
whoami = commandBuilder Whoami $ do
  init <$> readProcess (show Whoami) [] ""

dot :: String -> FilePath -> FilePath -> String -> IO ExitCode
dot fileName directory outDir fileType = commandBuilder Dot $ do
  (ec, _, _) <-
    readProcessWithExitCode
      (show Dot)
      [ "-T" ++ fileType
      , "-Gdpi=300"
      , infile
      , "-o"
      , outfile
      ]
      ""
  return ec
 where
  infile = directory </> (fileName ++ ".dot")
  outfile = outDir </> (fileName ++ ".png")

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

generateLatexTable :: String -> Algorithm -> Table -> String
generateLatexTable info algo (Table rows) =
  if length rows <= 20
    then
      unlines
        ( [ "\\begin{table}[htbp]"
          , "  \\centering"
          , "  \\begin{tabular}{|c|c|}"
          , "    \\hline"
          , "    Node & Distance \\\\"
          , "    \\hline"
          ]
            ++ rowStrings
            ++ [ "    \\hline"
               , "  \\end{tabular}"
               , "  \\caption{Distances calculated using " ++ show algo ++ ".}"
               , info
               , "\\end{table}"
               , "\\FloatBarrier"
               ]
        )
    else info
 where
  rowStrings = map rowToLatex rows
  rowToLatex (Row (Label lbl, Distance dist)) = "    " ++ lbl ++ " & " ++ show dist ++ " \\\\ \\hline"

generateLatexPreamble :: String -> String
generateLatexPreamble uname =
  unlines
    [ "\\documentclass{article}"
    , "\\usepackage{fancyhdr}"
    , "\\usepackage{placeins}"
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

mkFileSection :: String -> String
mkFileSection identifier = "\\section{File: " ++ identifier ++ "}"

latexEndDoc :: String
latexEndDoc = "\\end{document}"

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

createReportDir :: IO ()
createReportDir = do
  pwd <- getEnv "PWD"
  createDirectoryIfMissing True (pwd </> "results" </> "report")

createOutDirStructure :: String -> IO ()
createOutDirStructure graphFileName = do
  pwd <- getEnv "PWD"
  fileExists <- System.Directory.doesFileExist (pwd </> results)
  if fileExists
    then error $ redifyString "A regular file exists at $PWD/results please move this to continue ..."
    else
      mapM_
        (dirStructureBuilder pwd)
        [ "intermediteImages"
        , "intermediateGraphVizFiles"
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
  convert (pwd </> "results" </> name (head images)) pngFiles
  return ()
 where
  doDotAfterWrite :: Int -> VizImage -> FilePath -> IO FilePath
  doDotAfterWrite index img pwd = do
    createOutDirStructure $ name img
    dirPath <- graphDirResPath (name img)
    let intVizPath = dirPath </> "intermediateGraphVizFiles"
    let intPngPath = dirPath </> "intermediteImages"
    let baseFileName = name img ++ show index
    writeSingleImage baseFileName img intVizPath
    _ <- dot baseFileName intVizPath intPngPath "png"
    return (intPngPath </> (baseFileName ++ ".png"))

writeSingleImage :: String -> VizImage -> FilePath -> IO ()
writeSingleImage fileName img dirPath = do
  createDirectoryIfMissing True dirPath
  let filePath = dirPath </> (fileName ++ ".dot")
  writeFile filePath (show img)
  putStrLn $ "Wrote file: " ++ filePath

graphDirResPath :: FilePath -> IO FilePath
graphDirResPath name = do
  pwd <- getEnv "PWD"
  return $ pwd </> "results" </> name </> ""

-- End Grapviz Section

-- Command Line Section
helpScreen :: String
helpScreen =
  unlines
    [ "Usage: hsgraph [OPTIONS] (FILE|DIR)*"
    , ""
    , "Description:"
    , "    hsgraph is a command-line utility for parsing and analyzing graphs using Dijkstra's Algorithm, BFS and DFS."
    , "    It operates on file(s) or multiple files within a directory(s)."
    , "    It can also generate gifs following Dijkstra's Algorithm using GraphViz graphs."
    , "    The report and images for a given file are generated to $PWD/results/FILE_NAME/*"
    , ""
    , "Options:"
    , "  -h, --help"
    , "    Show this help message and exit."
    , ""
    , "  -g, --gif"
    , "    Generate a gif following Dijkstra's Algorithm for the specified file(s)."
    , ""
    , "  -r, --report"
    , "    Generate the report for the assignment on the specified files"
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
    , "  hsgraph myfile.txt /path/to/directory -s=5"
    , "  hsgraph /path/to/directory"
    , "  hsgraph /path/to/directory -g"
    , "  hsgraph myfile.txt -g"
    , "  hsgraph /path/to/directory -g --report"
    , "  hsgraph myfile.txt -g -r"
    , ""
    , "Note:"
    , "  Ensure that the file(s) or directory(s) supplied only contains files that hsgraph can parse."
    , "  Commands required for GIF generation is dot, convert"
    , "  Commands required for Report generation is dot, pdflatex"
    ]

data CmdPath
  = CmdDirectoryPath {getFilePath :: FilePath}
  | CmdFilePath {getFilePath :: FilePath}
  deriving (Eq, Show)

data CommandLineOption
  = CmdPath CmdPath
  | CmdHelp
  | CmdGif
  | CmdReport
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

cmdGif :: [String]
cmdGif = ["-g", "--gif"]

cmdReport :: [String]
cmdReport = ["-r", "--report"]

cmdStartNode :: String -> [String]
cmdStartNode s = (++ s) <$> ["-s=", "--start-node="]

strToOps :: String -> IO CommandLineOption
strToOps s
  | s `elem` cmdHelpList = return CmdHelp
  | s `elem` cmdGif = return CmdGif
  | s `elem` cmdReport = return CmdReport
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
    putStrLn helpScreen
    exitWith $ ExitFailure 1
  | otherwise = return ops
 where
  containsHelp = elem CmdHelp

allDirFiles :: [FilePath] -> IO [FilePath]
allDirFiles = allDirFiles' []
 where
  allDirFiles' :: [FilePath] -> [FilePath] -> IO [FilePath]
  allDirFiles' acc dirs =
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
            allDirFiles' (acc ++ map (x </>) files) xs
        )
          ()
   where
    isFileM :: FilePath -> IO Bool
    isFileM path = do
      result <- System.Directory.doesDirectoryExist path
      return (not result)
    dirFilter :: [FilePath] -> IO [FilePath]
    dirFilter = filterM isFileM

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

doReport :: Foldable t => [Map Algorithm (Graph, AlgorithmResult)] -> t CommandLineOption -> IO ()
doReport results ops = do
  when (CmdReport `elem` ops) $ do
    who <- whoami
    pwd <- getEnv "PWD"
    createReportDir
    let preamble = generateLatexPreamble who
        latex = preamble ++ foldl' (++) "" (map fromMap results) ++ latexEndDoc
    writeFile (filePath pwd) latex >> pdfLatex (filePath pwd) (dirPath pwd) Pdf >> return ()
 where
  filePath pwd = dirPath pwd </> "main.tex"
  dirPath pwd = pwd </> "results" </> "report"
  fromMap :: Map Algorithm (Graph, AlgorithmResult) -> String
  fromMap map = do
    let (Just (graph, dijk)) = Map.lookup Dijkstras map
    let (Just (_, bfs)) = Map.lookup BFS map
    let (Just (_, dfs)) = Map.lookup DFS map
    "\\newpage\n" ++ generateFileSection graph ++ generateAlgoSection dijk ++ generateAlgoSection bfs ++ generateAlgoSection dfs

  generateFileSection :: Graph -> String
  generateFileSection Graph{fileName = fileName} = mkFileSection fileName ++ "\n"

  generateAlgoSection :: AlgorithmResult -> String
  generateAlgoSection = \case
    DijkstrasResult r -> "\\pagestyle{empty}\n" ++ algoSubSection Dijkstras ++ genTable (numUpdates (DijkstrasResult r)) (DijkstrasResult r) ++ "\n"
    BFSResult r -> "\\pagestyle{empty}\n" ++ algoSubSection BFS ++ genTable (numUpdates (BFSResult r)) (BFSResult r) ++ "\n"
    DFSResult r -> "\\pagestyle{empty}\n" ++ algoSubSection DFS ++ genTable (numUpdates (DFSResult r)) (DFSResult r) ++ "\n"

  numUpdates :: AlgorithmResult -> String
  numUpdates = \case
    (DFSResult (_, Count c)) -> "Number of times visiting new nodes: " ++ show c ++ "\n"
    (BFSResult (_, Count c)) -> "Number of times visiting new nodes: " ++ show c ++ "\n"
    (DijkstrasResult (_, space)) -> "\\begin{enumerate}\n\\item Number of checks: " ++ show (length space) ++ "\n" ++ "\\item Number of shorter distances discovered: " ++ show (countRights space) ++ "\n\\end{enumerate}\n"

  genTable :: String -> AlgorithmResult -> String
  genTable updates = \case
    (DFSResult (table, _)) -> generateLatexTable updates DFS table
    (BFSResult (table, _)) -> generateLatexTable updates BFS table
    (DijkstrasResult (table, _)) -> generateLatexTable updates Dijkstras table

  algoSubSection :: Algorithm -> String
  algoSubSection = mkSubsection

doGif :: Foldable t => [(Graph, (Table, TransitionSpace))] -> t CommandLineOption -> IO ()
doGif results ops =
  when (CmdGif `elem` ops) $
    let images = map (\(x, (_, space)) -> transitionsToImages x (sortTransitionSpaceByCount space)) results
     in mapM_ (compileAllImages . reverse) images

--- End Command Line Section

-----------------Course Work------------------

-- Custom Types relating to the assignment
data Algorithm = Dijkstras | BFS | DFS deriving (Eq, Ord)

applyAlgorithm :: (a -> b) -> [a] -> [(a, b)]
applyAlgorithm f graphs = zip graphs (map f graphs)

data AlgorithmResult
  = BFSResult (Table, Count)
  | DFSResult (Table, Count)
  | DijkstrasResult (Table, TransitionSpace)

toAlgorithmMap :: [(Graph, (Table, Count))] -> [(Graph, (Table, Count))] -> [(Graph, (Table, TransitionSpace))] -> [Map Algorithm (Graph, AlgorithmResult)]
toAlgorithmMap bfsRes dfsRes dijkstrasRes =
  map createMapFromAlgo $ zip3 bfsRes dfsRes dijkstrasRes
 where
  createMapFromAlgo ((graph, (bfs, bfsCount)), (_, (dfs, dfsCount)), (_, (dijkstras, space))) =
    fromList
      [ (BFS, (graph, BFSResult (bfs, bfsCount)))
      , (DFS, (graph, DFSResult (dfs, dfsCount)))
      , (Dijkstras, (graph, DijkstrasResult (dijkstras, space)))
      ]

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

tuplesToTable :: [(Label, Int)] -> [Row]
tuplesToTable = map (\(x, y) -> Row (x, Distance y))

eitherTransitionCount :: Either Transition Transition -> Count
eitherTransitionCount t = count
 where
  count = transitionCount $ unwrapBothEither t

sortTransitionSpaceByCount :: TransitionSpace -> TransitionSpace
sortTransitionSpaceByCount = sortOn (count . transitionCount . unwrapBothEither)

-- End Recording Types

-- Dijkstra Specific
dijkstra :: Label -> Graph -> (Table, TransitionSpace)
dijkstra start graph =
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
        updatedDistances = foldl' (updateDistance nextLabel nextDistance) (table, space) (sortOn distanceToOther adjacents)
     in dijkstra' graph (nextLabel : visited) updatedDistances

updateDistance :: Label -> Distance -> (Table, TransitionSpace) -> Edge -> (Table, TransitionSpace)
updateDistance current (Distance currentDistance) (distanceTable, space) Edge{other = destination, distanceToOther = Distance weight}
  | newDistance < destinationDistance =
    ( newTable
    , Right
        Transition
          { from = From current
          , to = To destination
          , transitionDistance = Distance newDistance
          , transitionCount = currCount
          , state = newTable
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
  newTable = Table $ Row (destination, Distance newDistance) : delete (Row (destination, Distance destinationDistance)) (rows distanceTable)

-- End Dijkstra Stuff

-- Breadth/Depth First
breadthFirstSearch :: Label -> Graph -> (Table, Count)
breadthFirstSearch startLabel graph =
  let (visitedNodes, cnt) = breadthFirstSearch' (Count 0) [] [(startLabel, 0)]
   in (Table{rows = tuplesToTable visitedNodes}, cnt)
 where
  breadthFirstSearch' :: Count -> [(Label, Int)] -> [(Label, Int)] -> ([(Label, Int)], Count)
  breadthFirstSearch' count visited [] = (visited, count)
  breadthFirstSearch' (Count count) visited ((currentLabel, distance) : queue)
    | currentLabel `elem` map fst visited = breadthFirstSearch' (Count count) visited queue
    | otherwise =
      let currentNode = findNodeByLabel graph currentLabel
          newVisited = (currentLabel, distance) : visited
          adjacentNodes = unvisitedAdjacent visited currentNode
          newQueue = queue ++ [(label, distance + weight) | (label, weight) <- adjacentNodes]
       in breadthFirstSearch' (Count (count + 1)) newVisited newQueue

depthFirstSearch :: Label -> Graph -> (Table, Count)
depthFirstSearch startLabel graph =
  let (visitedNodes, cnt) = depthFirstSearch' (Count 0) [] [(startLabel, 0)]
   in (Table{rows = tuplesToTable visitedNodes}, cnt)
 where
  depthFirstSearch' :: Count -> [(Label, Int)] -> [(Label, Int)] -> ([(Label, Int)], Count)
  depthFirstSearch' count visited [] = (visited, count)
  depthFirstSearch' (Count count) visited ((currentLabel, dist) : stack)
    | currentLabel `elem` map fst visited = depthFirstSearch' (Count count) visited stack
    | otherwise =
      let currentNode = findNodeByLabel graph currentLabel
          newVisited = (currentLabel, dist) : visited
          adjacentNodes = unvisitedAdjacent visited currentNode
          newStack = [(label, dist + weight) | (label, weight) <- adjacentNodes] ++ stack
       in depthFirstSearch' (Count (count + 1)) newVisited newStack

findNodeByLabel :: Graph -> Label -> Node
findNodeByLabel graph lbl = head $ filter (\node -> identifier node == lbl) (nodes graph)

unvisitedAdjacent :: [(Label, Int)] -> Node -> [(Label, Int)]
unvisitedAdjacent visited node =
  [(lbl, weight) | Edge{other = lbl, distanceToOther = Distance weight} <- adjacent node, lbl `notElem` map fst visited]

-- End Breadth/Depth First

-------------- End Course Work Section ---------------

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

  let bfsRes = applyAlgorithm (breadthFirstSearch startLabel) graphs
  let dfsRes = applyAlgorithm (depthFirstSearch startLabel) graphs
  let dijkstrasRes = applyAlgorithm (dijkstra startLabel) graphs
  let algoMaps = toAlgorithmMap bfsRes dfsRes dijkstrasRes

  -- print algoMaps
  doGif dijkstrasRes ops
  doReport algoMaps ops

  putStrLn "Done!"
