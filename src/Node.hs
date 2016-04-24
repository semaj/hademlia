module Node where
import qualified Constants as C
import           Message
import           Query
import           RoutingData
import qualified Utils as U

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Time.Clock as Clock

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
                 , nPendingStores :: [(ID, T.Text)]
                 , nPendingFinds :: [ID]
                 , nLastSeen :: HM.HashMap ID Clock.UTCTime
                 , nLastSent :: HM.HashMap ID Message
                 , nNodeInfos :: HM.HashMap ID IPInfo
                 , nPrintBuffer :: [T.Text]
                 }

routeToQueries :: [(QueryID, QueryMessageResponse)] -> Queries -> Queries
routeToQueries qmrPairs queries = L.foldl' myInsert queries qmrPairs
  where myInsert accQueries (queryID, qmr) = HM.adjust (insertIncoming qmr) queryID accQueries

delegateIncomingResponses :: Node -> Node
delegateIncomingResponses n@Node{..} = n { nFindValueQueries = routeToQueries translated nFindValueQueries
                                         , nFindNodeQueries = routeToQueries translated nFindNodeQueries }
  where translated = bulkToQueryMessageResponse nIncoming

retrieveNodes :: Queries -> [(ID, [ID])]
retrieveNodes = M.catMaybes . fmap fetchFoundNodes . HM.elems

clearFoundNodes :: Queries -> Queries
clearFoundNodes = HM.filter (M.isJust . fetchFoundNodes)

clearFinishedQueries :: Node -> Node
clearFinishedQueries n@Node{..} = n { nFindNodeQueries = clearFoundNodes nFindNodeQueries }
