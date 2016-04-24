module Node where
import qualified Constants as C
import           Message
import           Query
import           RoutingData
import qualified Utils as U

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Time.Clock as CLOCK

type Queries = HM.HashMap QueryID Query
type KVStore = HM.HashMap ID T.Text

data Node = Node { nPort :: Port
                 , nIP :: IP
                 , nNodeID :: ID
                 , nTree :: Tree
                 , nStore :: KVStore
                 , nFindValueQueries :: Queries
                 , nFindNodeQueries :: Queries
                 , nIncoming :: [Message]
                 , nOutgoing :: [Message]
                 , nUserStores :: [(ID, T.Text)]
                 , nLastSeen :: HM.HashMap ID CLOCK.UTCTime
                 , nLastSent :: HM.HashMap ID Message
                 , nNodeInfos :: HM.HashMap ID IPInfo
                 }

routeToQueries :: [(QueryID, QueryMessageResponse)] -> Queries -> Queries
routeToQueries qmrPairs queries = L.foldl' myInsert queries qmrPairs
  where myInsert accQueries (queryID, qmr) = if HM.member queryID accQueries
                                             then HM.adjust (insertIncoming qmr) queryID accQueries
                                             else error "routToQueries wtf"

delegateIncoming :: Node -> Node
delegateIncoming Node{..} = undefined
