import CGVector

v1 = Vec [2, 3, 4]
v2 = Vec [2, 3, 2]
p1 = Vec [1, 0, 0]
p2 = Vec [0, 0, 0]
p3 = Vec [0, 1, 0]
emptyV = Vec [] :: Vec Int

main :: IO ()
main = do
  print $ mag v1
  print $ mag v2
  print $ mag v1 * mag v2
  print $ v1 .^ v2
  print $ (v1 .^ v2) / (mag v1 * mag v2)
  print $ acos $ (v1 .^ v2) / (mag v1 * mag v2)
  print $ angle v1 v2
  print $ angleP p1 p2 p3
