# CS438 Search

## About 
CLI program written in haskell to compare graph algorithms performance and use cases. Written with no external libraries because cabal/stack package manager is not available for use in the assignment testing environment. 

##  Instructor building from the extracted zip
Shouldn't require any special compiler flags. All code is in the `Main.hs` file. The file is broken into comment blocks that wrap the code to outline  distinctually different areas of the program. The block with the comment "Course Work" is all the logic pertaining to where the base assignment requirements are (ie Dijkstras, BFS, DFS).
```bash
ghc -o hsgraph ./app/Main.hs
```
## Instructor Usage
- All commands needed for full generation are available and tested on home.cs.siue.edu
- The following command will generate a single PDF report and gif(s) to accompany each file found/provided to the command
- WARNING
  - Do not try and run the gif generation on big graphs unless you have lots of ram
    - `convert` command used to build the GIF will steal all ram and cause a crash
  - Tested gif generation on siue home server with supplied `graphPosLittle` and it worked fine
- The report generation generates distance tables for graphs <= 20 nodes. The table spans off the page if I don't do this and dynamically fixing is tricky
```bash
./hsgraph /path/to/my/(graphFile|graphFilledDir) /optionally/other/file(s) -r -g
```

### Chat GPT
 - [Transcript for generating the help text](https://chat.openai.com/share/9af8ee57-d54a-4ad2-a799-8bac8a9a7f88)
 - [Transcript for generating latex table from custom table type](https://chat.openai.com/share/9ba74a22-018a-4302-abd3-81778c888b32)

### Shell Command Dependencies
- Report Generation
    - `pdflatex`
- Graph Generation
    - GraphViz Package
        - `dot` -> Builds a "prettier" graph but ignores the len attribute on edges
        - `convert` -> Builds gifs from the images
- Other Builtins/Standard
    - `which`
    - `whoami`

## Clone + Cabal Usage 
```bash
git clone git@github.com:BrianTipton1/hsgraph.git
cabal run hsgraph -- "/path/to/my/(graphFile|graphFilledDir) -r -g"
```

### Help Text
```bash
./hsgraph --help
```
```
Usage: hsgraph [OPTIONS] (FILE|DIR)*

Description:
    hsgraph is a command-line utility for parsing and analyzing graphs using Dijkstra's Algorithm, BFS and DFS.
    It operates on file(s) or multiple files within a directory(s).
    It can also generate gifs following Dijkstra's Algorithm using GraphViz graphs.
    The report and images for a given file are generated to $PWD/results/FILE_NAME/*

Options:
  -h, --help
    Show this help message and exit.

  -g, --gif
    Generate a gif following Dijkstra's Algorithm for the specified file(s).

  -r, --report
    Generate the report for the assignment on the specified files

  -s=2, --start-node=2
    Optionally supply a node to start algorithms from

Arguments:
  FILE
    Required. The path to a file(s) and or a directory(s) to be parsed.

Examples:
  hsgraph myfile.txt
  hsgraph myfile.txt /path/to/directory -s=5
  hsgraph /path/to/directory
  hsgraph /path/to/directory -g
  hsgraph myfile.txt -g
  hsgraph /path/to/directory -g --report
  hsgraph myfile.txt -g -r

Note:
  Ensure that the file(s) or directory(s) supplied only contains files that hsgraph can parse.
  Commands required for GIF generation is dot, convert
  Commands required for Report generation is dot, pdflatex
```

### For future me
- Resources Used
  - [graphviz](https://graphviz.org/)
- Interesting Packages
  - [cmdargs](https://hackage.haskell.org/package/cmdargs)
  - [graphviz](https://hackage.haskell.org/package/graphviz)
  - [HaTeX](https://hackage.haskell.org/package/HaTeX)
- Things I wanted to do but couldn't do to various factors (Time/Runtime Environment/Accessibility)
  - Not have all code in one file
  - Better GraphViz implementation
    - Conversion of `Graph` type to `VizImage` is sloppy
    - Better structure of the `VizImage` and child types
  - Possibly some redundant code if genericization was thought through better
  - Parallization of image compilation using `dot`
  - Different GIF conversion implementation to accommodate a larger number of images
  - Nicer report generation. Also sloppy