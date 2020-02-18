{-# LANGUAGE OverloadedStrings #-}

module GraphvizBlock (graphvizBlock) where

import Data.Digest.Pure.SHA (sha256, showDigest)
import Data.Text (pack, unpack)
import Data.Text.Lazy.Encoding ( encodeUtf8 )
import Data.Text.Lazy ( fromStrict )
import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)
import Text.Pandoc.Definition

graphvizBlock :: Maybe Format -> Block -> IO Block
graphvizBlock (Just format) (CodeBlock (ident, classes, keyvals) content)
    | "graphviz" `elem` classes =
        case format of
          Format "html" ->
              do
                (ec, out, err) <- readProcessWithExitCode "dot" ["-Tsvg", "-K" <> layout] $ unpack content
                return $ case ec of
                           ExitSuccess -> RawBlock (Format "html")
                                 ("<div id=\"" <> ident <> "\" class=\"graphviz\">" <> pack out <> "</div>")
                           _ -> CodeBlock (ident, classes, keyvals) $ pack err
          Format "latex" ->
              do
                (_, out, err) <- readProcessWithExitCode
                                  "dot2tex" ["--figonly", "--progoptions=-K" <> layout] $ unpack content
                return $ if null err
                         then RawBlock (Format "latex") $ pack out
                         else CodeBlock (ident, classes, keyvals) $ pack err
          _ ->
              do
                (ec, _, err) <- readProcessWithExitCode
                                  "dot" ["-Tpng", "-K" <> layout, "-o" <> filename] $ unpack content
                return $ case ec of
                           ExitSuccess -> Para [Image (ident, [], []) [Str content] (pack filename, ident)]
                           _ -> CodeBlock (ident, classes, keyvals) $ pack err
              where
                uniq = (showDigest . sha256 . encodeUtf8 . fromStrict) (pack layout <> "/" <> content)
                filename = "graphviz-" <> uniq <> ".png"
    where
      layout = unpack $
          case lookup "layout" keyvals of
            Just x -> x
            _ -> "dot"
graphvizBlock _ x = return x
