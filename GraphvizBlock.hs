module GraphvizBlock (graphvizBlock) where

import System.Exit
import System.Process
import Text.Pandoc

graphvizBlock :: Maybe Format -> Block -> IO Block
graphvizBlock (Just format) (CodeBlock (id, classes, keyvals) content)
    | elem "graphviz" classes =
        case format of
          Format "html" ->
              do
                (ec, out, err) <- readProcessWithExitCode "dot" ["-Tsvg", "-K" ++ layout] content
                return $ case ec of
                           ExitSuccess -> RawBlock (Format "html")
                                 ("<div id=\"" ++ id ++ "\" class=\"graphviz\">" ++ out ++ "</div>")
                           _ -> CodeBlock (id, classes, keyvals) err
          _ -> return $ CodeBlock (id, classes, keyvals) content
    where
      layout =
          case lookup "layout" keyvals of
            Just x -> x
            _ -> "dot"
graphvizBlock _ x = return x
