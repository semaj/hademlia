module RoutingData where
import qualified Data.ByteString as B
import           Utils

type ID = String
type IPAddress = B.ByteString
type Port = B.ByteString
type IPInfo = (IPAddress, Port)
type NodeInfo = (ID, IPInfo)
type KBucket = [NodeInfo]
  
data Tree = Leaf KBucket | Branch { zero :: Tree, one :: Tree }

kbFull :: KBucket -> Bool
kbFull = ((<) 20) . length

kbContainsID :: KBucket -- ^ does this kbucket contain 
             -> ID      -- ^ this id?
             -> Bool    -- ^ yes or no
kbContainsID kb nodeID = any (((==) nodeID) . fst) kb

closestKBucket :: Tree    -- ^ The tree we're searching through
               -> ID      -- ^ Look for a kbucket closest to this ID
               -> KBucket -- ^ The closest (XOR distance) kbucket to the ID
closestKBucket (Leaf kbucket) _ = kbucket
closestKBucket (Branch _ o) ('1':restID) = closestKBucket o restID
closestKBucket (Branch z _) ('0':restID) = closestKBucket z restID
closestKBucket (Branch _ _) [] = error "closest: tree is taller than id is long"

insert :: Tree     -- ^ current tree
       -> ID       -- ^ the current node's ID
       -> NodeInfo -- ^ the node we are inserting
       -> ID       -- ^ the current (being walked) prefix
       -> Tree     -- ^ new tree
insert (Leaf kbucket) u w suffix
    | kbFull kbucket && kbContainsID kbucket u = splitBucket (apnd kbucket w) $ length w - length suffix
    | kbFull kbucket = Leaf kbucket
    | otherwise = Leaf $ apnd kbucket w
insert (Branch z o) u w (firstID:restID)
  | firstID == '1' = Branch z (insert o u w restID)
  | firstID == '0' = Branch (insert z u w restID) o
insert (Branch _ _) _ _ [] = error "insert: tree is taller than id is long"

splitBucket :: KBucket  -- ^ Kbucket we're splitting
            -> Int      -- ^ index to split the bucket on (if splitting)
            -> Tree     -- ^ Leaf if not split, Branch if split
splitBucket kb splitIndex
  | kbFull kb = Branch (splitBucket zeros newIndex) (splitBucket ones newIndex)
  | otherwise = Leaf kb
     where (zeros, ones) = span (\(nid, _) -> (nid!!splitIndex) == '0') kb
           newIndex = splitIndex + 1
