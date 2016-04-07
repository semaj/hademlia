module RoutingDataSpec where
import RoutingData
import Test.Hspec

nodeA = ("A", ("0.0.0.0", "1337"))
nodeB = ("B", ("0.0.0.0", "1337"))
nodeC = ("C", ("0.0.0.0", "1337"))
treeA = Branch (Branch (Leaf [nodeA])
                       (Leaf [nodeB]))
               (Leaf [nodeC])

spec :: Spec
spec =
  describe "closestKBucket" $ do
    it "finds nodeB in treeA" $ do
      closestKBucket treeA "01" `shouldBe` [nodeB]
    it "finds nodeC in treeA" $ do
      closestKBucket treeA "1" `shouldBe` [nodeC]
    it "finds nodeA in treeA" $ do
      closestKBucket treeA "00" `shouldBe` [nodeA]
