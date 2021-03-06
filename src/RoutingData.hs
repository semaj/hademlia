module RoutingData where
import qualified Utils as U

import qualified Data.List as L
import qualified Data.Text as T


-- Handy aliases
type ID = String
type IP = String
type QueryID = String
type Port = Int
type IPInfo = (IP, Port)
type NodeInfo = (ID, IPInfo)
type KBucket = [ID]

data Tree = Leaf KBucket | Branch { zero :: Tree, one :: Tree }
  deriving (Eq, Show)

kbFull :: KBucket -> Bool
kbFull = ((<) 5) . length

kbContainsID :: KBucket -- ^ Does this kbucket contain ...
             -> ID      -- ^ This ID?
             -> Bool    -- ^ True or False?
kbContainsID kb nodeID = any ((==) nodeID) kb

closestKBucket :: Tree    -- ^ The tree we're searching through
               -> ID      -- ^ Look for a kbucket closest to this ID
               -> KBucket -- ^ The closest (XOR distance) kbucket to the ID
closestKBucket (Leaf kbucket) _ = kbucket
closestKBucket (Branch _ o) ('1':restID) = closestKBucket o restID
closestKBucket (Branch z _) ('0':restID) = closestKBucket z restID
closestKBucket (Branch _ _) [] = error "closest: tree is taller than id is long"

-- | Inserts a node into our routing tree, performing kbucket splits as necessary.
-- if the node ID was already in the k-bucket, remove it and add it to the end.
insert :: Tree     -- ^ Current Tree
       -> ID       -- ^ The current node's ID
       -> ID       -- ^ The node we are inserting
       -> Tree     -- ^ New Tree
insert t nid wid = insert' t nid wid wid

insert' :: Tree -> ID -> ID -> String -> Tree
insert' (Leaf kbucket) u wid suffix
    | kbFull kbucket && kbContainsID kbucket u =
        splitBucket (U.apnd (L.delete wid kbucket) wid) $ length wid - length suffix
    | kbFull kbucket = Leaf kbucket
    | otherwise = Leaf $ U.apnd (L.delete wid kbucket) wid
insert' (Branch z o) u w ('1':restID) = Branch z (insert' o u w restID)
insert' (Branch z o) u w ('0':restID) = Branch (insert' z u w restID) o
insert' (Branch _ _) _ _ [] = error "insert: tree is taller than id is long"

-- | Here, we split kbuckets if they're full. Kademlia dictates that you split
-- kbuckets if they're full and they contain your ID, but we will assume
-- consumers of this function have determined that the bucket does contain our ID.
-- If it's not full, we don't have to modify it and can simply return the kbucket as a Leaf.
-- Start from the given index. Continue to split the kbucket into subtrees
-- (walking the IDs) until none of the kbuckets are full.
splitBucket :: KBucket  -- ^ Kbucket we're splitting
            -> Int      -- ^ The index to start comparing each ID on
            -> Tree     -- ^ Leaf if not split, Branch if split
splitBucket kb splitIndex
  | kbFull kb = Branch (splitBucket zeros newIndex) (splitBucket ones newIndex)
  | otherwise = Leaf kb
     where (zeros, ones) = L.partition (\nid -> (nid!!splitIndex) == '0') kb
           newIndex = splitIndex + 1

-- | Order is unimportant, since it will probably be sorted anyway
treeToList :: Tree -> [ID]
treeToList (Leaf kb) = kb
treeToList (Branch a b) = treeToList a ++ treeToList b

nodeDistance :: ID -> ID -> Int
nodeDistance x y = U.bitsToDec $ zipWith U.xor x y
