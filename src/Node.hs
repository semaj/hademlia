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

retrieveNodes :: Queries -> HM.HashMap ID [ID]
retrieveNodes = HM.fromList . M.catMaybes . fmap fetchFoundNodes . HM.elems

clearFoundNodes :: Queries -> Queries
clearFoundNodes = HM.filter (M.isJust . fetchFoundNodes)

matchValueToNodeIDs :: [(ID, T.Text)] -> HM.HashMap ID [ID] -> [([ID], T.Text)]
matchValueToNodeIDs pending targetToNodes = M.catMaybes $ L.foldl' locate [] pending
  where locate acc (target, value) = U.apnd acc $ fmap (U.asnd value) $ HM.lookup target targetToNodes

storesToExecute :: Node -> [([ID], T.Text)]
storesToExecute Node{..} = matchValueToNodeIDs nPendingStores $ retrieveNodes nFindNodeQueries

clearFinishedQueries :: Node -> Node
clearFinishedQueries n@Node{..} = n { nFindNodeQueries = clearFoundNodes nFindNodeQueries }

removeFindValueQuery :: Node -> QueryID -> Node
removeFindValueQuery n@Node{..} qID = n { nFindValueQueries = HM.delete qID nFindValueQueries }
