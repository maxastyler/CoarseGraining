module CoarseGraining where

import CGVector
import PDBParser

data Pair = Pair {atoms :: (Atom, Atom), name :: String, contacts :: Int, rIDs :: (Int, Int)}
  deriving (Show, Eq)

data Element = Carbon | Flourine | Hydrogen |
               Nitrogen | Oxygen | Sulphur deriving (Show, Eq)

mass :: Fractional a => Element -> a
mass e = case e of
  Carbon -> 12.0107
  Flourine -> 18.9984
  Hydrogen -> 1.00794
  Nitrogen -> 14.0067
  Oxygen -> 15.9994
  Sulphur -> 32.065

coarseGrainAtoms :: [Atom] -> [Atom]
coarseGrainAtoms = undefined
