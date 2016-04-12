module RoutingData where
import qualified Data.List as L
import qualified Data.Text as T
import           Utils

-- Handy aliases
type ID = String
type IP = String
type Port = String
type IPInfo = (IP, Port)
type NodeInfo = (ID, IPInfo)
type KBucket = [NodeInfo]

data Tree = Leaf KBucket | Branch { zero :: Tree, one :: Tree }
  deriving (Eq, Show)

kbFull :: KBucket -> Bool
kbFull = ((<) 5) . length

kbContainsID :: KBucket -- ^ Does this kbucket contain ...
             -> ID      -- ^ This ID?
             -> Bool    -- ^ True or False?
kbContainsID kb nodeID = any (((==) nodeID) . fst) kb

closestKBucket :: Tree    -- ^ The tree we're searching through
               -> ID      -- ^ Look for a kbucket closest to this ID
               -> KBucket -- ^ The closest (XOR distance) kbucket to the ID
closestKBucket (Leaf kbucket) _ = kbucket
closestKBucket (Branch _ o) ('1':restID) = closestKBucket o restID
closestKBucket (Branch z _) ('0':restID) = closestKBucket z restID
closestKBucket (Branch _ _) [] = error "closest: tree is taller than id is long"

-- | Inserts a node into our routing tree, performing kbucket splits as necessary.
insert :: Tree     -- ^ Current Tree
       -> ID       -- ^ The current node's ID
       -> NodeInfo -- ^ The node we are inserting
       -> Tree     -- ^ New Tree
insert t nid winfo@(wid, _) = insert' t nid winfo wid

insert' :: Tree -> ID -> NodeInfo -> String -> Tree
insert' (Leaf kbucket) u w@(wid, _) suffix
    | kbFull kbucket && kbContainsID kbucket u =
        splitBucket (apnd kbucket w) $ length wid - length suffix
    | kbFull kbucket = Leaf kbucket
    | otherwise = Leaf $ apnd kbucket w
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
     where (zeros, ones) = L.partition (\(nid, _) -> (nid!!splitIndex) == '0') kb
           newIndex = splitIndex + 1

charToBit :: Char -> Int
charToBit '1' = 1
charToBit '0' = 0
charToBit _ = error "The character you're trying to turn into a bit isn't a 1 || 0. Something weird happened."

xor :: Char -> Char -> Int
xor x y = abs $ charToBit x - charToBit y

nodeDistance :: ID -> ID -> Int
nodeDistance x y = bitsToDec $ zipWith xor x y

bitsToDec :: [Int] -> Int
bitsToDec = L.foldl' (\acc x -> acc * 2 + x) 0
