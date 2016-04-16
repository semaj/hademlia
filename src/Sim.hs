module Sim where
import           Message
import           Node
import           RoutingData
import           Utils

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Data.Time.Clock
import           System.Random

idBits = 10

idRange :: (Int, Int)
idRange = (0, (2 ^ idBits) - 1)

data Sim = Sim { nodes :: HM.HashMap IPInfo Node
               , queue :: [Message]
               , gen :: StdGen
               }

testNode :: ID -> ID -> UTCTime -> Node
testNode ours theirs now = Node { port = 0
                                , ip = ours
                                , nodeID = ours
                                , tree = Leaf [theirs]
                                , store = HM.empty
                                , qstates = HM.empty
                                , incoming = []
                                , outgoing = []
                                , userStores = []
                                , lastTime = HM.fromList [(theirs, now)]
                                , lastSent = HM.empty
                                , idToInfo = HM.fromList [(theirs, (theirs, 0))]
                                }


randomID :: RandomGen g => g -> (ID, g)
randomID gen = (zeroPad idBits $ decToBitString rID, g)
  where (rID, g) = randomR idRange gen

randLog :: (RandomGen g, Random a, Show a) => (a, a) -> g -> String -> IO (a, g)
randLog range@(lo, hi) gen logMe = do
  putStrLn $ "Generating " ++ logMe ++ "between " ++ (show lo) ++ " - " ++ (show hi)
  return $ randomR range gen

randomNode :: RandomGen g => g -> [Node] -> (Node, g)
randomNode g nodes =
  case maybeNode of
    Nothing -> error $ "Node list no index " ++ (show i)
    Just node -> (node, g')
  where range = (0, length nodes - 1)
        (i, g') = randomR range g
        maybeNode = get nodes i

initNodes :: RandomGen g => g -> UTCTime -> (HM.HashMap ID Node, g)
initNodes gen now = (HM.fromList [(aID, nodeA), (bID, nodeB)], gen'')
   where (aID, gen') = randomID gen
         (bID, gen'') = randomID gen'
         nodeA = testNode aID bID now
         nodeB = testNode bID aID now

execute :: Node -> Node
execute node = node

start :: RandomGen g => g -> IO ()
start gen = do
  now <- getCurrentTime
  let (nodes, gen') = initNodes gen now
  run gen' nodes [] [] HM.empty

data Command = Put { key :: ID
                   , value :: T.Text
                   , triggered :: UTCTime
                   , success :: Bool
                   }
             | Get { key :: ID
                   , triggered :: UTCTime
                   , success :: Bool
                   }

run :: RandomGen g => g         -- ^ The random number generator
       -> HM.HashMap ID Node    -- ^ ID -> Node with that ID
       -> [Message]             -- ^ Message queue between nodes
       -> [Command]             -- ^ Commands sent
       -> HM.HashMap ID T.Text  -- ^ Reference hashmap (DHT map should emulate this)
       -> IO ()
run g nodes messages commands refStore = do
  putStrLn "sup"
