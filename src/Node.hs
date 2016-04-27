module Node where
import           Message
import           Query
import           RoutingData as RD
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
                 , nPrintBuffer :: [String]
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

clearFinishedFindNodes :: Node -> Node
clearFinishedFindNodes n@Node{..} = n { nFindNodeQueries = clearFoundNodes nFindNodeQueries }

removeFindValueQuery :: Node -> QueryID -> Node
removeFindValueQuery n@Node{..} qID = n { nFindValueQueries = HM.delete qID nFindValueQueries }

pruneFindValueRIncoming :: Node -> Node
pruneFindValueRIncoming n@Node{..} = n { nIncoming = filter (not . isFindValueR) nIncoming }

findValueRsOnly :: [Message] -> [(QueryID, ID, T.Text)]
findValueRsOnly = M.catMaybes . fmap cleanFindValueR

showValueTriples :: [(QueryID, ID, T.Text)] -> [String]
showValueTriples = fmap (show . U.sndthd)

removeFindValueRQueries :: Node -> Node
removeFindValueRQueries n@Node{..} = L.foldl' removeFindValueQuery n fvrs
  where fvrs = fmap U.tfst $ findValueRsOnly nIncoming

bufferFoundValues :: Node -> Node
bufferFoundValues n@Node{..} = n { nPrintBuffer = nPrintBuffer' }
  where nPrintBuffer' = nPrintBuffer ++ (showValueTriples . findValueRsOnly $ nIncoming)

foundValues :: Node -> Node
foundValues = pruneFindValueRIncoming . removeFindValueRQueries . bufferFoundValues

slurpSourcesIntoTree :: [Message] -> ID -> Tree -> Tree
slurpSourcesIntoTree incoming myID tree = L.foldl' folder tree sources
  where sources = fmap src incoming
        folder accTree source = RD.insert accTree myID source

slurpSources :: Node -> Node
slurpSources n@Node{..} = n { nTree = slurpSourcesIntoTree nIncoming nNodeID nTree }

slurpSourceInfosIntoMap :: [Message] -> HM.HashMap ID IPInfo -> HM.HashMap ID IPInfo
slurpSourceInfosIntoMap incoming infos = L.foldl' folder infos incoming
  where folder accMap mess = HM.insert (src mess) (srcInfo mess) accMap

slurpSourceInfos :: Node -> Node
slurpSourceInfos n@Node{..} = n { nNodeInfos = slurpSourceInfosIntoMap nIncoming nNodeInfos }

-- last seen, last sent, findr results into nodeinfos

slurpNewNodeInfosIntoMap :: [Message] -> HM.HashMap ID IPInfo -> HM.HashMap ID IPInfo
slurpNewNodeInfosIntoMap incoming infos = L.foldl' folder infos $ concat $ fmap getNodeInfos incoming
  where folder accMap info = HM.insert (fst info) (snd info) accMap

slurpNewNodeInfos :: Node -> Node
slurpNewNodeInfos n@Node{..} = n { nNodeInfos = slurpNewNodeInfosIntoMap nIncoming nNodeInfos }

slurpNewNodeIDsIntoTree :: [Message] -> ID -> Tree -> Tree
slurpNewNodeIDsIntoTree incoming myID tree = L.foldl' folder tree $ concat $ fmap getNodeInfos incoming
  where folder accTree info = RD.insert accTree myID (fst info)

slurpNewNodeIDs :: Node -> Node
slurpNewNodeIDs n@Node{..} = n { nTree = slurpNewNodeIDsIntoTree nIncoming nNodeID nTree }

slurp :: Node -> Node
slurp = slurpNewNodeIDs . slurpNewNodeInfos . slurpSources . slurpSourceInfos
