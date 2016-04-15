module Node where
import           Message
import           RoutingData

import qualified Data.HashMap.Strict as HM
import qualified Data.Heap as H
import qualified Data.Text as T
import           Data.Time.Clock
import           Network.Socket


data Node = Node { port :: Port
                 , ip :: IP
                 , nodeID :: ID
                 , tree :: Tree
                 , store :: HM.HashMap ID T.Text
                 , qstates :: HM.HashMap QueryID QueryState
                 , incoming :: [Message]
                 , outgoing :: [Message]
                 , userStores :: [(ID, T.Text)]
                 , lastTime :: HM.HashMap ID UTCTime
                 , lastSent :: HM.HashMap ID Message
                 , idToInfo :: HM.HashMap ID IPInfo
                 }

data NodeHeapInfo = NHI { distance :: Int
                        , node :: NodeInfo
                        , responded :: Bool
                        }

instance Eq NodeHeapInfo where
  x == y = (distance x) == (distance y)

instance Ord NodeHeapInfo where
  x <= y = (distance x) <= (distance y)

data QueryResult = Failure T.Text | FoundValue T.Text | FoundNode NodeInfo | NotDone

data QueryState = QueryState { heap :: H.MinHeap NodeHeapInfo
                             , queryID :: QueryID
                             , qtarget :: ID
                             , qresult :: QueryResult
                             , qincoming :: [Message]
                             , qoutgoing :: [Message]
                             }
