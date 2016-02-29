module RoutingData where
import qualified Node

type IPAddress = String
type Port = String
type NodeInfo = (Node.ID, IPAddress, Port)
type KBucket = [NodeInfo]
  
data Tree = Leaf KBucket | Branch { zero :: Tree, one :: Tree }

closestKBucket :: Tree -> Node.ID -> KBucket
closestKBucket (Branch _ _) [] = [] -- this should be impossible!
closestKBucket (Leaf kbucket) _ = kbucket
closestKBucket (Branch _ o) ('1':restID) = closestKBucket o restID
closestKBucket (Branch z _) ('0':restID) = closestKBucket z restID

 
