module CoarseGraining where

import CGVector
import PDBParser
import GHC.Exts (groupWith)
import Data.List
import Data.Maybe (catMaybes)

data Contact = Bond deriving (Show, Eq, Ord)

data Pair = Pair {beads :: (Bead, Bead), contacts :: Int, contactType :: Contact}
  deriving (Eq)

--Gets a pair of residue ids from a Pair of Bead(s)
rIDs :: Pair -> (Int, Int)
rIDs pr = (b1, b2)
  where b1 = bResId $ fst $ beads pr
        b2 = bResId $ snd $ beads pr

pairName :: Pair -> String
pairName pair = (show $ bType b1) ++ (show $ bResId b1) ++ "-" ++ (show $ bType b2) ++ (show $ bResId b2)
  where beadies = beads pair
        b1 = fst beadies
        b2 = snd beadies

data BeadType = CAlpha | Sidechain deriving (Eq)

instance Show BeadType where
  show CAlpha = "CA"
  show Sidechain = "SC"

-- Calpha/Sidechain bead - similar to atoms but contains a list of atoms it is derived from
data Bead = Bead {
  bId :: Int, -- The id of the bead
  bType :: BeadType, -- The type of the bead (calpha, sidechain)
  residue :: Amino, -- The residue type of the bead
  bChainId :: Char, -- The id of the chain (A..Z)
  bResId :: Int, -- The residue id of the bead
  bPos :: Vec Double, -- The position of the bead
  nHeavy :: Int, -- The weight(?) of the bead
  bAtoms :: [Atom], -- The list of atoms the bead contains
  bSigma :: Double -- The temperature coefficient of the bead
                 } deriving (Eq, Ord)

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
sortIntoResidues atoms = let grouped = groupWith (\xi -> (chainID xi, resSeq xi)) atoms
                         in map (break (not . isBackbone)) grouped

calcSigma :: Double -> Double
calcSigma numHeavy = 4*(numHeavy/4)**(1/3)

-- Function to make the Calpha bead with a given id from a list of calpha atoms
getCAlpha :: [Atom] -> Int -> Maybe Bead
getCAlpha ats aid = let mCAlpha = find (\xi -> PDBParser.name xi == "CA") ats
                   in
                     case mCAlpha of
                       Nothing -> Nothing
                       Just calpha -> Just $ Bead aid CAlpha (resName calpha) (chainID calpha) (resSeq calpha) (Vec [x calpha, y calpha, z calpha]) 4 ats (calcSigma 4)

getSideChain :: [Atom] -> Int -> Maybe Bead
getSideChain [] _ = Nothing
getSideChain ats aid = Just $ Bead aid Sidechain (resName at) (chainID at) (resSeq at) (Vec [x at, y at, z at]) (length ats) ats (calcSigma $ fromIntegral $ length ats)
  where at = head ats

-- Renumber the list of beads
reNumBeads :: [Bead] -> Int -> [Bead]
reNumBeads [] _ = []
reNumBeads (b:bs) i = b { bId = i } : (reNumBeads bs (i+1))

-- Used to create a list of the pairs of atoms that need to be iterated over
createPairs :: [a]->[(a, a)]
createPairs [] = []
createPairs (b:bs) = (map (\c -> (b, c)) bs) ++ (createPairs bs)

-- Turn a list of atoms into a list of beads
coarseGrainAtoms :: [Atom] -> [Bead]
coarseGrainAtoms ats = let residues = sortIntoResidues ats
                           makeBeads :: ([Atom], [Atom]) -> [Maybe Bead]
                           makeBeads (a1, a2) = [getCAlpha a1 0, getSideChain a2 0]
                           unNumChain = foldl (++) [] $ map makeBeads residues
                       in
                         reNumBeads (catMaybes unNumChain) 0

beadContainsId :: Int -> Bead -> Bool
beadContainsId atId bd = any (\at -> serial at == atId) (bAtoms bd)

zipAll :: [a]->[b]->[(a, b)]
zipAll = \a b -> (a >>= \ai -> (map (\bi -> (ai, bi)) b))

contactToPair :: (Int, Int) -> [Bead] -> Pair
contactToPair (c1, c2) bds = Pair (b1, b2) 1 Bond
  where Just b1 = find (beadContainsId c1) bds
        Just b2 = find (beadContainsId c2) bds

genPairs :: [Bead] -> [(Int, Int)] -> Maybe [Pair]
genPairs bds cts= let groupedContactPairs = groupWith id $ map contactToPair cts
                  in
                    undefined
