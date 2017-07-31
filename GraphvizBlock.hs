module GraphvizBlock (graphvizBlock) where

import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Digest.Pure.SHA (sha256, showDigest)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)
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
          Format "latex" ->
              do
                (ec, out, err) <- readProcessWithExitCode
                                  "dot2tex" ["--figonly", "--progoptions=-K" ++ layout] content
                return $ if length err == 0
                         then RawBlock (Format "latex") out
                         else CodeBlock (id, classes, keyvals) err
          _ ->
              do
                (ec, out, err) <- readProcessWithExitCode
                                  "dot" ["-Tpng", "-K" ++ layout, "-o" ++ filename] content
                return $ case ec of
                           ExitSuccess -> Para [Image (id, [], []) [Str content] (filename, id)]
                           _ -> CodeBlock (id, classes, keyvals) err
              where
                uniq = ((showDigest . sha256 . fromString) (layout ++ "/" ++ content))
                filename = "graphviz-" ++ uniq ++ ".png"
    where
      layout =
          case lookup "layout" keyvals of
            Just x -> x
            _ -> "dot"
graphvizBlock _ x = return x
