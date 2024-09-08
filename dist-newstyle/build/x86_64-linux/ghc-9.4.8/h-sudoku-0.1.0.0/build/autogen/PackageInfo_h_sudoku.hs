{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_h_sudoku (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "h_sudoku"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "A sudoku Solver and a Sudoku Generator"
copyright :: String
copyright = ""
homepage :: String
homepage = "https://www.linkedin.com/in/gabriel-emannuel-gama-andrade-325b42268/"
