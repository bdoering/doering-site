module Abbreviations (abbreviationFilter) where
import qualified Data.Map    as M
import           Hakyll (replaceAll)

abbreviationFilter :: String -> String
abbreviationFilter = replaceAll "%[a-zA-Z0-9_]*" newnaming
  where
    newnaming matched = case M.lookup cleaned abbreviations of
                          Nothing -> cleaned
                          Just v -> "<abbr title=\"" ++ v ++ "\">" ++ cleaned ++ "</abbr>"
        where cleaned = tail matched

-- List of all abbreviations used on site
abbreviations :: M.Map String String
abbreviations = M.fromList
                [ ("ERCS", "equivalent radar cross section")
                , ("SAR", "synthetic aperture radar")
                , ("SVG", "scalable vector graphics")
                ]
                                                            
