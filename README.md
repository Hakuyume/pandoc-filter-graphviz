# Graphviz filter for Pandoc

This filter provides Graphviz code block support.

## Features
- This filter embeds svg image directly when the output format is html.
- The layout engine can be specified in block attributes.
- For wrong inputs, it outputs error message. `pandoc` command does not fail.

## Requirements
- pandoc
- graphviz

## Build
```
$ cd pandoc-graphviz
$ ghc --make Main.hs -o pandoc-graphviz
```

Note: `-dynamic` option is required in some environments.

## Usage
```
$ pandoc -F ./pandoc-graphviz example.md -o example.html
```
