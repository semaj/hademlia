module Sim where
import qualified Dummy as D
import           Message
import           Node
import           RoutingData
import           Utils

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Data.Time.Clock
import           System.Random

idBits :: Int
idBits = 10

idRange :: (Int, Int)
idRange = (0, (2 ^ idBits) - 1)

data Sim = Sim { nodes :: HM.HashMap IPInfo Node
               , queue :: [Message]
               , gen :: StdGen
               }

testNode :: ID -> ID -> UTCTime -> Node
testNode ours theirs now = Node { nPort = 0
                                , nIP = ours
                                , nNodeID = ours
                                , nTree = Leaf [theirs]
                                , nStore = HM.empty
                                , nFindValueQueries = HM.empty
                                , nFindNodeQueries = HM.empty
                                , nIncoming = []
                                , nOutgoing = []
                                , nPendingStores = []
                                , nPendingFinds = []
                                , nLastSeen = HM.fromList [(theirs, now)]
                                , nLastSent = HM.empty
                                , nNodeInfos = HM.fromList [(theirs, (theirs, 0))]
                                , nPrintBuffer = []
                                }


intToID :: Int ->  ID
intToID i = zeroPad idBits $ decToBitString i

randLog :: (Random a, Show a) => String -> (a, a) -> IO a
randLog logMe r@(lo, hi) = do
  putStrLn $ "Generating " ++ logMe ++ " between " ++ (show lo) ++ " - " ++ (show hi)
  getStdRandom $ randomR r

getNode :: Int -> [Node] -> Node
getNode i nodes =
  case get nodes i of
    Nothing -> error $ "Node list no index " ++ (show i)
    Just node -> node

initNodes :: Int -> Int -> UTCTime -> HM.HashMap ID Node
initNodes i1 i2 now = HM.fromList [(aID, nodeA), (bID, nodeB)]
   where aID = intToID i1
         bID = intToID i2
         nodeA = testNode aID bID now
         nodeB = testNode bID aID now

execute :: Node -> Node
execute node = node

start :: StdGen -> IO ()
start gen = do
  now <- getCurrentTime
  setStdGen gen
  id1 <- randLog "node A ID" idRange
  id2 <- randLog "node B ID" idRange
  let nodes = initNodes id1 id2 now
  run nodes [] [] HM.empty

data Command = Put { key :: ID
                   , value :: T.Text
                   , triggered :: UTCTime
                   , success :: Bool
                   }
             | Get { key :: ID
                   , triggered :: UTCTime
                   , success :: Bool
                   }

run :: HM.HashMap ID Node       -- ^ ID -> Node with that ID
       -> [Message]             -- ^ Message queue between nodes
       -> [Command]             -- ^ Commands sent
       -> HM.HashMap ID T.Text  -- ^ Reference hashmap (DHT map should emulate this)
       -> IO ()
run nodes messages commands refStore = do
  putStrLn "sup"
