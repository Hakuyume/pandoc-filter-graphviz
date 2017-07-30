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
          _ ->
              do
                (ec, out, err) <- readProcessWithExitCode
                                  "dot" ["-T" ++ ext, "-K" ++ layout, "-o" ++ filename] content
                return $ case ec of
                           ExitSuccess -> Para [Image (id, [], []) [Str content] (filename, id)]
                           _ -> CodeBlock (id, classes, keyvals) err
              where
                ext = case format of
                        Format "latex" -> "pdf"
                        _ -> "png"
                filename = ((showDigest . sha256 . fromString) (layout ++ "/" ++ content)) ++ "." ++ ext
    where
      layout =
          case lookup "layout" keyvals of
            Just x -> x
            _ -> "dot"
graphvizBlock _ x = return x
