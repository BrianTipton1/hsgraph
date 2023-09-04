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
- The following command will generate a single PDF report and graph(s) to accompany each file found/provided to the command
```bash
./hsgraph /path/to/my/(graphFile|graphFilledDir) /optionally/other/file(s) -g
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
    - `uuidgen`
    - `which`

## Clone + Cabal Usage 
```bash
git clone git@github.com:BrianTipton1/hsgraph.git
cabal run hsgraph -- "/path/to/my/(graphFile|graphFilledDir)"
```

### Help Text
```bash
./hsgraph --help
```
```
Usage: hsgraph [OPTIONS] (FILE|DIR)*

Description:
  hsgraph is a command-line utility for parsing and analyzing graphs using Dijkstra's Algorithm, BFS and DFS. It operates on file(s) or multiple files within a directory(s). Optionally, it can also generate GraphViz graphs. The report and images are generated to $PWD/results

Options:
  -h, --help
    Show this help message and exit.

  -g, --graph
    Generate VizGraphs for the selected file(s).

  -ng, --no-gif
    Do not generate GIFs from the image files. This option can only be used in conjunction with the -g/--graph option.

  -s=2, --start-node=2
    Optionally supply a node to start algorithms from

Arguments:
  FILE
    Required. The path to a file(s) and or a directory(s) to be parsed.

Examples:
  hsgraph myfile.txt
  hsgraph myfile.txt /path/to/directory
  hsgraph /path/to/directory
  hsgraph /path/to/directory -g
  hsgraph myfile.txt -g
  hsgraph /path/to/directory -g --no-gif
  hsgraph myfile.txt -g --no-gif

Note:
  Ensure that the file(s) or directory(s) supplied only contains files that hsgraph can parse.
  The -ng/--no-gif option can only be used in conjunction with the -g/--graph option.
```

### For future me if looking for libs/resources I found
- Resources
  - [graphviz](https://graphviz.org/)
- Packages
  - [cmdargs](https://hackage.haskell.org/package/cmdargs)
  - [graphviz](https://hackage.haskell.org/package/graphviz)
  - [HaTeX](https://hackage.haskell.org/package/HaTeX)
