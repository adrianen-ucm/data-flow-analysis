# Live variable analysis

This program expects some (simplified) 'while' source code via
stdin, sends it to an [API](https://github.com/manuelmontenegro/while_parser_api) 
for obtaining its CFG and performs a live variable analysis.

## Build requirements

- GHC >= 8.10.7
- Cabal >= 3.4

## Usage example

You can try with the example in [example.while](./example.while). The
first time it will take a while because it has to perform a fresh build.

```sh
# Omit -v0 if you want to observe the build process status
cat example.while | cabal run -v0
```

which should output

```
Waiting for the source code input...
Obtaining the program CFG...
Live variable analysis:

Block 1:
  Content: c:= 10
  Successors: 2
  In analysis: {}
  Out analysis: {c}
Block 2:
  Content: y:= 1
  Successors: 3
  In analysis: {c}
  Out analysis: {c, y}
Block 3:
  Content: If (1 <= c)
  Successors: 4, 6
  In analysis: {c, y}
  Out analysis: {c, y}
Block 4:
  Content: y:= (y * c)
  Successors: 5
  In analysis: {c, y}
  Out analysis: {c, y}
Block 5:
  Content: c:= (c - 1)
  Successors: 3
  In analysis: {c, y}
  Out analysis: {c, y}
Block 6:
  Content: res:= y
  Successors: None
  In analysis: {y}
  Out analysis: {}
```
You can also use directly:

```sh
cabal run -v0
```

and, after seeing

```
Waiting for the source code input...
```

type by hand the source code and press `CTRL+D` when finished.