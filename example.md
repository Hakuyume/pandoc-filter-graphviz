## examples of Graphviz code block

### basic usage
```graphviz
digraph {
  a -> b;
  b -> c;
}
```

### specify layout engine
```{.graphviz layout=neato}
digraph {
  a -> b;
  b -> c;
}
```

### error in code
```graphviz
digraph {
  a -- b;
  b -- c;
}
```
