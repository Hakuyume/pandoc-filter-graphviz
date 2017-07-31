# A Pandoc filter for Graphviz

This filter provides Graphviz code block support.

## Features
- This filter changes the way to embed image according to the output format.
    - For html, it embeds svg element directly. In this case, no temporary files are created.
    - For pdf/LaTeX, it saves image as pdf.
    - For other formats, it saves image as png.
- The layout engine can be specified in block attributes.
- For wrong inputs, it embeds the error message as a code block. `pandoc` command does not fail.

## Requirements
- pandoc
- graphviz

## Build
```
$ cd pandoc-filter-graphviz
$ ghc --make Main.hs -o pandoc-filter-graphviz
```

Note: `-dynamic` option is required in some environments.

## Usage
```
$ pandoc -F ./pandoc-filter-graphviz example.md -o example.html
```
