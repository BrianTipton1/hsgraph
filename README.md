# CS438 Search

##  Instructor building from the extracted zip
> Shouldn't require any special compiler flags
```bash
ghc -o hsgraph ./app/Main.hs
```
## Instructor Usage
```bash
./hsgraph "/path/to/my/(graphFile|graphFilledDir)"
```

### Chat GPT
 - [Transcript for generating the help text](https://chat.openai.com/share/9af8ee57-d54a-4ad2-a799-8bac8a9a7f88)

## Clone + Cabal Usage 
```bash
git clone git@github.com:BrianTipton1/hsgraph.git
cabal run hsgraph -- "/path/to/my/(graphFile|graphFilledDir)"
```