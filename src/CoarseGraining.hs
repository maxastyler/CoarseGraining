module CoarseGraining where

import CGVector
import PDBParser

data Pair = Pair {atoms :: (Atom, Atom), name :: String, contacts :: Int, rIDs :: (Int, Int)}
  deriving (Show, Eq)

coarseGrainAtoms :: [Atom] -> [Atom]
coarseGrainAtoms = undefined
