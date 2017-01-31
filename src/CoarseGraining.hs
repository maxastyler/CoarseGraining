module CoarseGraining where

import CGVector
import PDBParser
import GHC.Exts (groupWith)

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

isBackbone :: Atom -> Bool
isBackbone a = let at = PDBParser.name a
               in at=="CA" || at == "N" || at == "C" || at == "O"

-- Takes a list of atoms and produces a list of tuples containing (c-alpha atoms, sidechain atoms (if any))
sortIntoResidues :: [Atom] -> [([Atom], [Atom])]
sortIntoResidues atoms = let grouped = groupWith (\x -> (chainID x, resSeq x)) atoms
                         in map (break (not . isBackbone)) grouped

coarseGrainAtoms :: [Atom] -> [Atom]
coarseGrainAtoms = undefined
