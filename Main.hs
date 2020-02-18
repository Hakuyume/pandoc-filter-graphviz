import Text.Pandoc.JSON
import GraphvizBlock

main :: IO ()
main = toJSONFilter graphvizBlock
