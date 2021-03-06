module Abbreviations (abbreviationFilter) where
import qualified Data.Map as M
import           Hakyll   (replaceAll)

abbreviationFilter :: String -> String
abbreviationFilter = replaceAll "%[a-zA-Z0-9_]+" newnaming
  where
    newnaming matched = case M.lookup cleaned abbreviations of
                          Nothing -> cleaned
                          Just v -> "<abbr title=\"" ++ v ++ "\">" ++ cleaned ++ "</abbr>"
        where cleaned = tail matched

-- List of all abbreviations used on site
abbreviations :: M.Map String String
abbreviations = M.fromList
                [ ("CAD", "computer-aided design")
                , ("CEOS", "Committee on Earth Observation Satellites")
                , ("ERCS", "equivalent radar cross section")
                , ("GUI", "graphical user-interface")
                , ("JSON", "JavaScript Object Notation")
                , ("MCMC", "Monte Carlo Markov Chain")
                , ("MCMCs", "Monte Carlo Markov Chains")
                , ("SAR", "synthetic aperture radar")
                , ("SVG", "scalable vector graphics")
                ]

