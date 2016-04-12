module RoutingDataSpec where
import RoutingData
import Test.Hspec

nodeA = "A"
nodeB = "B"
nodeC = "C"
treeA = Branch (Branch (Leaf [nodeA])
                       (Leaf [nodeB]))
               (Leaf [nodeC])
nodeD = "01010"
nodeE = "01110"
nodeF = "10000"
nodeG = "01000"
nodeH = "11000"
nodeI = "11100"
defghi = [nodeD, nodeE, nodeF, nodeG, nodeH, nodeI]
nodeJ = "01100"
nodeK = "01011"
nodeL = "01111"
degjkl = [nodeD, nodeE, nodeG, nodeJ, nodeK, nodeL]
treeDEGJKL = Branch (Branch (Leaf [])
                            (Branch (Leaf [nodeD, nodeG, nodeK])
                                    (Leaf [nodeE, nodeJ, nodeL])))
                    (Leaf [])
nodeM = "01101"
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

  describe "nodeDistance" $ do
    it "returns the distance between two nodes by XORing their respective IDs" $ do
      nodeDistance "1111" "1111" `shouldBe` 0
      nodeDistance "1011" "1101" `shouldBe` 6

  describe "bitsToDec" $ do
    it "converts a string of bits to its base 10 Int equivalent" $ do
      bitsToDec [1,1,0] `shouldBe` 6
