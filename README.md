# A Pandoc filter for Graphviz

This filter provides Graphviz code block support.

## Features
- This filter changes the way to embed image according to the output format.
    - For html, it embeds svg elements directly.
    - For pdf/LaTeX, it embeds TikZ figures directly.
    - For other formats, it saves png images in working directory and embeds their links.
- The layout engine can be specified in block attributes.
- For wrong inputs, it embeds the error message as a code block. `pandoc` command does not fail.

## Requirements
- pandoc
- graphviz
- dot2tex (pdf/LaTeX support)

## Setup

### Build filter
```
$ cd pandoc-filter-graphviz
$ ghc --make Main.hs -o pandoc-filter-graphviz
```
Note: `-dynamic` option is required in some environments.

### Add TikZ to LaTeX template
You can skip this step if you do not use pdf/LaTeX.

- Copy default template
    ```
    $ mkdir -p ~/.pandoc/templates
    $ pandoc -D latex > ~/.pandoc/templates/default.latex
    ```

- Edit `~/.pandoc/templates/default.latex`
    ```latex
    ...
    \usepackage{tikz} % insert this line

    \begin{document}
    ...
    ```

## Usage
```
$ pandoc -F ./pandoc-filter-graphviz example.md -o example.html
```
