import CGVector
import PDBParser

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

v1 = Vec [2, 3, 4]
v2 = Vec [2, 3, 2]
p1 = Vec [1, 0, 0]
p2 = Vec [0, 0, 0]
p3 = Vec [0, 1, 0]
emptyV = Vec [] :: Vec Int

main :: IO ()
main = do
  print $ angleP p1 p2 p3
