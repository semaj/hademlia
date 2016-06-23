module Node where
import qualified Constants as C
import qualified Message as MSG
import qualified Query as Q
import qualified RoutingData as RD
import qualified Utils as U

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Time.Clock as Clock

-- | Individual queries we are currently running
-- and waiting for. FIND_NODE, FIND_VALUES
type Queries = HM.HashMap RD.QueryID Q.Query

-- | The actual map of ID -> Value
type KVStore = HM.HashMap RD.ID T.Text

-- | The Node's state
data Node = Node { nPort :: RD.Port -- | The port we're running on
                 , nIP :: RD.IP -- | Our IP
                 , nNodeID :: RD.ID -- | Our ID
                 , nTree :: RD.Tree -- | Our current routing tree
                 , nStore :: KVStore -- | Our current data store
                 , nFindValueQueries :: Queries -- | Our in-progress FIND_VALUE queries
                 , nFindNodeQueries :: Queries -- | Our in-progress FIND_NODE queries
                 , nIncoming :: [MSG.Message] -- | Messages we have just received next IO cycle
                 , nOutgoing :: [MSG.Message] -- | Messages we are planning to send out next IO cycle
                 , nPendingStores :: [(RD.ID, T.Text)] -- | Store commands a user has executed on this node
                 , nPendingFinds :: [RD.ID] -- | Find commands a user has executed on this node
                 , nLastSeen :: HM.HashMap RD.ID Clock.UTCTime -- | Keeps track of the last time we've seen every node
                 , nLastSent :: HM.HashMap RD.ID MSG.Message -- | Keeps track of the last message we sent to each node
                 , nNodeInfos :: HM.HashMap RD.ID RD.IPInfo -- | Map of IP/port info for every node we've seen
                 , nPrintBuffer :: [String] -- | Things to print to the screen (like a found value for the user operating this node)
                 }

-- | Delegates QueryMessageResponses to the appropriate query, based on query ID
routeToQueries :: [(RD.QueryID, Q.QueryMessageResponse)] -- | The QueryID -> QMR pairs
               -> Queries -- | The queries to delegate message responses to
               -> Queries -- | Updated queries
routeToQueries qmrPairs queries = L.foldl' myInsert queries qmrPairs
  where myInsert accQueries (queryID, qmr) =
          HM.adjust (Q.insertIncoming qmr) queryID accQueries

-- | Node-wrapped `routeToQueries`
delegateIncomingResponses :: Node -> Node
delegateIncomingResponses n@Node{..}
   = n { nFindValueQueries = routeToQueries translated nFindValueQueries
    , nFindNodeQueries = routeToQueries translated nFindNodeQueries }
  where translated = MSG.bulkToQueryMessageResponse nIncoming

-- | Pull out all nodes found in any FOUND_NODE query's responses.
-- Keyed off the target.
retrieveNodes :: Queries -> HM.HashMap RD.ID [RD.ID]
retrieveNodes = HM.fromList . M.catMaybes . fmap Q.fetchFoundNodes . HM.elems

-- | Filters for find_node queries that found nodes
successfulFindNodeQueries :: Queries -> Queries
successfulFindNodeQueries = HM.filter (M.isJust . Q.fetchFoundNodes)

-- |
matchKeyToNodeIDs :: [(RD.ID, T.Text)]
                    -> HM.HashMap RD.ID [RD.ID]
                    -> [([RD.ID], T.Text)]
matchKeyToNodeIDs pending targetToNodes
   = M.catMaybes $ L.foldl' locate [] pending
  where locate acc (target, value) =
          U.apnd acc $ fmap (U.arvs value) $ HM.lookup target targetToNodes

storesToExecute :: Node -> [([RD.ID], T.Text)]
storesToExecute Node{..}
  = matchKeyToNodeIDs nPendingStores $ retrieveNodes nFindNodeQueries

-- | Wrapped `successfulFindNodeQueries`
clearFinishedFindNodes :: Node -> Node
clearFinishedFindNodes n@Node{..}
  = n { nFindNodeQueries = successfulFindNodeQueries nFindNodeQueries }

removeFindValueQuery :: Node -> RD.QueryID -> Node
removeFindValueQuery n@Node{..} qID
  = n { nFindValueQueries = HM.delete qID nFindValueQueries }

pruneFindValueRIncoming :: Node -> Node
pruneFindValueRIncoming n@Node{..}
  = n { nIncoming = filter (not . MSG.isFindValueR) nIncoming }

findValueRsOnly :: [MSG.Message] -> [(RD.QueryID, RD.ID, T.Text)]
findValueRsOnly = M.catMaybes . fmap MSG.cleanFindValueR

showValueTriples :: [(RD.QueryID, RD.ID, T.Text)] -> [String]
showValueTriples = fmap (show . U.sndthd)

removeFindValueRQueries :: Node -> Node
removeFindValueRQueries n@Node{..} = L.foldl' removeFindValueQuery n fvrs
  where fvrs = fmap U.tfst $ findValueRsOnly nIncoming

bufferFoundValues :: Node -> Node
bufferFoundValues n@Node{..} = n { nPrintBuffer = nPrintBuffer' }
  where nPrintBuffer'
          = nPrintBuffer ++ (showValueTriples . findValueRsOnly $ nIncoming)

-- | Handles values we've received in responses, removing them
-- for future processing
foundValues :: Node -> Node
foundValues = pruneFindValueRIncoming . removeFindValueRQueries . bufferFoundValues

slurpSourcesIntoTree :: [MSG.Message] -> RD.ID -> RD.Tree -> RD.Tree
slurpSourcesIntoTree incoming myID tree = L.foldl' folder tree sources
  where sources = fmap MSG.src incoming
        folder accTree source = RD.insert accTree myID source

slurpSources :: Node -> Node
slurpSources n@Node{..} = n { nTree = slurpSourcesIntoTree nIncoming nNodeID nTree }

slurpSourceInfosIntoMap :: [MSG.Message] -> HM.HashMap RD.ID RD.IPInfo
                        -> HM.HashMap RD.ID RD.IPInfo
slurpSourceInfosIntoMap incoming infos = L.foldl' folder infos incoming
  where folder accMap mess = HM.insert (MSG.src mess) (MSG.srcInfo mess) accMap

slurpSourceInfos :: Node -> Node
slurpSourceInfos n@Node{..}
  = n { nNodeInfos = slurpSourceInfosIntoMap nIncoming nNodeInfos }

-- last seen, last sent, findr results into nodeinfos

slurpNewNodeInfosIntoMap :: [MSG.Message] -> HM.HashMap RD.ID RD.IPInfo
                         -> HM.HashMap RD.ID RD.IPInfo
slurpNewNodeInfosIntoMap incoming infos
  = L.foldl' folder infos $ concat $ fmap MSG.getNodeInfos incoming
  where folder accMap info = HM.insert (fst info) (snd info) accMap

slurpNewNodeInfos :: Node -> Node
slurpNewNodeInfos n@Node{..}
  = n { nNodeInfos = slurpNewNodeInfosIntoMap nIncoming nNodeInfos }

slurpNewNodeIDsIntoTree :: [MSG.Message] -> RD.ID -> RD.Tree -> RD.Tree
slurpNewNodeIDsIntoTree incoming myID tree
  = L.foldl' folder tree $ concat $ fmap MSG.getNodeInfos incoming
  where folder accTree info = RD.insert accTree myID (fst info)

slurpNewNodeIDs :: Node -> Node
slurpNewNodeIDs n@Node{..}
  = n { nTree = slurpNewNodeIDsIntoTree nIncoming nNodeID nTree }

-- | Pull any new information from nodes we receiving messages from into
-- the respective information hashmaps
slurp :: Node -> Node
slurp = slurpNewNodeIDs . slurpNewNodeInfos . slurpSources . slurpSourceInfos

respondFN :: HM.HashMap RD.ID RD.IPInfo -> RD.ID -> RD.IPInfo
          -> Clock.UTCTime -> MSG.Message ->  Maybe MSG.Message
respondFN nodeInfos myID myInfo now MSG.FindNode{..}
  = Just $ MSG.FindNodeR myID myInfo src results now mRound mID qID
  where sorter (a, _) (b, _) = compare (RD.nodeDistance target a)
                                       (RD.nodeDistance target b)
        results = take C.k $ L.sortBy sorter $ HM.toList nodeInfos
respondFN _ _ _ _ _ = Nothing

-- slurp find values that we know the value for (stores must happen before this)

respondFindNodeMs :: [MSG.Message] -> RD.ID -> RD.IPInfo -> Clock.UTCTime
                  -> HM.HashMap RD.ID RD.IPInfo -> RD.Tree -> [MSG.Message]
respondFindNodeMs incoming myID myInfo now nodeInfos tree
  = M.catMaybes $ fmap (respondFN nodeInfos myID myInfo now) incoming

respondFindNodes :: Node -> Node
