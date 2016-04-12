module Node where
import qualified Data.HashMap.Strict as HM
import qualified Data.Heap as H
import qualified Data.Text as T
import           Message
import           Network.Socket
import           RoutingData

data Node = Node { port :: Port
                 , ip :: IP
                 , nodeID :: ID
                 , tree :: Tree
                 , store :: HM.HashMap ID T.Text
                 , qstate :: Maybe QueryState
                 , incoming :: [Message]
                 , outgoing :: [Message]
                 , userStores :: [(ID, T.Text)]
                 , userFinds :: [T.Text]
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

data QueryState = QueryState { kheap :: H.MinHeap NodeHeapInfo
                             , qtarget :: ID
                             , qresult :: QueryResult
                             }
