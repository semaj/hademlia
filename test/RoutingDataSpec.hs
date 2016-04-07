module RoutingDataSpec where
import RoutingData
import Test.Hspec

mkNdI id name = (id, (name, name))
info = ("0.0.0.0", "1337")

nodeA = ("A", info)
nodeB = ("B", info)
nodeC = ("C", info)
treeA = Branch (Branch (Leaf [nodeA])
                       (Leaf [nodeB]))
               (Leaf [nodeC])
nodeD = mkNdI "01010" "D"
nodeE = mkNdI "01110" "E"
nodeF = mkNdI "10000" "F"
nodeG = mkNdI "01000" "G"
nodeH = mkNdI "11000" "H"
nodeI = mkNdI "11100" "I"
defghi = [nodeD, nodeE, nodeF, nodeG, nodeH, nodeI]
nodeJ = mkNdI "01100" "J"
nodeK = mkNdI "01011" "K"
nodeL = mkNdI "01111" "L"
degjkl = [nodeD, nodeE, nodeG, nodeJ, nodeK, nodeL]
treeDEGJKL = Branch (Branch (Leaf [])
                            (Branch (Leaf [nodeD, nodeG, nodeK])
                                    (Leaf [nodeE, nodeJ, nodeL])))
                    (Leaf [])
nodeM = mkNdI "01101" "M"
treeDEGJKLM = Branch (Branch (Leaf [])
                             (Branch (Leaf [nodeD, nodeG, nodeK])
                                     (Leaf [nodeE, nodeJ, nodeL, nodeM])))
                     (Leaf [])
treeDEGJKLlimit = Branch (Branch (Leaf [])
                                 (Leaf [nodeD, nodeE, nodeG, nodeJ, nodeK, nodeL]))
                         (Leaf [])


spec :: Spec
spec = do
  describe "closestKBucket" $ do
    it "finds nodeB in treeA" $ do
      closestKBucket treeA "01" `shouldBe` [nodeB]
    it "finds nodeC in treeA" $ do
      closestKBucket treeA "1" `shouldBe` [nodeC]
    it "finds nodeA in treeA" $ do
      closestKBucket treeA "00" `shouldBe` [nodeA]
  describe "splitBucket" $ do
    it "returns a leaf when not full" $ do
      splitBucket [nodeA, nodeB, nodeC] 0 `shouldBe` Leaf [nodeA, nodeB, nodeC]
      splitBucket [] 0 `shouldBe` Leaf []
    it "returns a branch when full" $ do
      splitBucket defghi 0 `shouldBe` Branch (Leaf [nodeD, nodeE, nodeG])
                                             (Leaf [nodeF, nodeH, nodeI])
    it "returns branch when full (nested)" $ do
      splitBucket degjkl 0 `shouldBe` treeDEGJKL
  describe "insert" $ do
    it "simply appends when target kbucket is not full" $ do
      insert treeDEGJKL "01010" nodeM `shouldBe` treeDEGJKLM
    it "drops when the target kbucket is full" $ do
      insert treeDEGJKLlimit "10000" nodeM `shouldBe` treeDEGJKLlimit
    it "splits target bucket when full and contains our node" $ do
      insert treeDEGJKLlimit "01010" nodeM `shouldBe` treeDEGJKLM
