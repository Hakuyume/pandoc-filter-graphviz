module GititGraphviz (plugin) where

import Network.Gitit.Interface
import GraphvizBlock

plugin = mkPageTransformM $ liftIO . graphvizBlock (Just (Format "html"))
