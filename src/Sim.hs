module Sim where
import           Message
import           Node
import           RoutingData
import           Utils

import qualified Data.HashMap.Strict as HM
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

initNodes :: RandomGen g => g -> UTCTime -> ((Node, Node), g)
initNodes gen now = ((nodeA, nodeB), gen'')
   where (aID, gen') = randomID gen
         (bID, gen'') = randomID gen'
         nodeA = testNode aID bID now
         nodeB = testNode bID aID now

start :: StdGen -> IO ()
start gen = putStrLn "Yeah, I simulate things"
