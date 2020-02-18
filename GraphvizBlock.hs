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
graphvizBlock (Just format) (CodeBlock (id, classes, keyvals) content)
    | elem "graphviz" classes =
        case format of
          Format "html" ->
              do
                (ec, out, err) <- readProcessWithExitCode "dot" ["-Tsvg", "-K" <> layout] $ unpack content
                return $ case ec of
                           ExitSuccess -> RawBlock (Format "html")
                                 ("<div id=\"" <> id <> "\" class=\"graphviz\">" <> pack out <> "</div>")
                           _ -> CodeBlock (id, classes, keyvals) $ pack err
          Format "latex" ->
              do
                (ec, out, err) <- readProcessWithExitCode
                                  "dot2tex" ["--figonly", "--progoptions=-K" <> layout] $ unpack content
                return $ if length err == 0
                         then RawBlock (Format "latex") $ pack out
                         else CodeBlock (id, classes, keyvals) $ pack err
          _ ->
              do
                (ec, out, err) <- readProcessWithExitCode
                                  "dot" ["-Tpng", "-K" <> layout, "-o" <> unpack filename] $ unpack content
                return $ case ec of
                           ExitSuccess -> Para [Image (id, [], []) [Str content] (filename, id)]
                           _ -> CodeBlock (id, classes, keyvals) $ pack err
              where
                uniq = ((pack . showDigest . sha256 . encodeUtf8 . fromStrict) (pack layout <> "/" <> content))
                filename = "graphviz-" <> uniq <> ".png"
    where
      layout = unpack $
          case lookup "layout" keyvals of
            Just x -> x
            _ -> "dot"
graphvizBlock _ x = return x
