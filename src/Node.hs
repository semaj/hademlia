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

-- | Node ID -> IP metadata
type NodeInfos = HM.HashMap RD.ID RD.IPInfo

-- | The Node's state
data Node =
  Node
  { nPort :: RD.Port -- | The port we're running on
  , nIP :: RD.IP -- | Our IP
  , nNodeID :: RD.ID -- | Our ID
  , nTree :: RD.Tree -- | Our current routing tree
  , nStore :: KVStore -- | Our current data store
  , nFindValueQueries :: Queries -- | Our in-progress FIND_VALUE queries
  , nFindNodeQueries :: Queries -- | Our in-progress FIND_NODE queries
  , nIncoming :: [MSG.Message] -- | Messages we have just received next IO cycle
  , nOutgoing :: [MSG.Message] -- | Messages we plan to send out next IO cycle
  , nPendingStores :: [(RD.ID, T.Text)] -- | Store commands user executed on node
  , nPendingFinds :: [RD.ID] -- | Find commands a user has executed on this node
  , nLastSeen :: HM.HashMap RD.ID Clock.UTCTime -- | When we last saw each node
  , nLastSent :: HM.HashMap RD.ID MSG.Message -- | What we last sent to each node
  , nNodeInfos :: NodeInfos -- | Map of IP/port info for every node we've seen
  , nPrintBuffer :: [String] -- | Things ready to print for user
  }

-- ||| Handle FIND_NODE responses

-- | Delegates QueryMessageResponses to the appropriate query, based on query ID
routeToQueries :: [(RD.QueryID, Q.QueryMessageResponse)] -- | The QueryID -> QMR pairs
               -> Queries -- | The queries to delegate message responses to
               -> Queries -- | Updated queries
routeToQueries qmrPairs queries = L.foldl' myInsert queries qmrPairs
  where myInsert accQueries (queryID, qmr) =
          HM.adjust (Q.insertIncoming qmr) queryID accQueries

-- | Sends incoming FIND_NODE responses to their appropriate queries.
routeFindNodeRs :: Node -> Node
routeFindNodeRs n@Node{..}
   = n { nFindValueQueries = routeToQueries translated nFindValueQueries
       , nFindNodeQueries = routeToQueries translated nFindNodeQueries }
  where translated = MSG.bulkToQueryMessageResponse nIncoming

-- | Filters for find_node queries that are not finished
unfinishedFindNodeQueries :: Queries -> Queries
unfinishedFindNodeQueries = HM.filter (M.isNothing . Q.fetchFoundNodes)

-- | Wrapped `unfinishedFindNodeQueries`
clearFinishedFindNodes :: Node -> Node
clearFinishedFindNodes n@Node{..}
  = n { nFindNodeQueries = unfinishedFindNodeQueries nFindNodeQueries }

handleFindNodeRs :: Node -> Node
handleFindNodeRs = routeFindNodeRs

-- ||| Store key,value pairs in the network once we have found the
-- appropriate node locations

-- | Pull out all nodes found in any FOUND_NODE query's responses.
-- Keyed off the target.
retrieveFoundNodes :: Queries -> HM.HashMap RD.ID [RD.ID]
retrieveFoundNodes =
  HM.fromList . M.catMaybes . fmap Q.fetchFoundNodes . HM.elems

-- | Finds the IDs for the nodes at which we will store
-- each pending store data (key, value) from the given
-- target -> node ids hashmap, pulled from FIND_NODE responses
matchStoresToTargets :: [(RD.ID, T.Text)] -- | Pending stores : (Key,Value)
                    -> HM.HashMap RD.ID [RD.ID] -- | Target -> K closest nodes, from FIND_NODE queries
                    -> [(RD.ID, T.Text, [RD.ID])]
matchStoresToTargets pending targetToNodes
   = M.catMaybes $ L.foldl' locate [] pending
  where locate acc (target, value) =
          U.apnd acc $ fmap ((,,) target value) $ HM.lookup target targetToNodes

-- TODO: Create fully wrapped version of this
-- | Wrapped `matchStoresToTargets`. Finds targets
storesToExecute :: Node -> [(RD.ID, T.Text, [RD.ID])]
storesToExecute Node{..}
  = matchStoresToTargets nPendingStores $ retrieveFoundNodes nFindNodeQueries

constructStores :: Node -> T.Text -> Clock.UTCTime -> Node
constructStores n@Node{..} mID now = n { nOutgoing = nOutgoing ++ storeMs }
  where ready = storesToExecute n
        storeMs = MSG.bulkStores nNodeID (nIP, nPort) now mID ready

-- ||| Handle FIND_VALUE responses

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
handleFindValueRs :: Node -> Node
handleFindValueRs =
  pruneFindValueRIncoming . removeFindValueRQueries . bufferFoundValues

-- ||| Pulling node data from messages into state (such as IP addresses & IDs
-- ||| from both message sources and results.

-- | Add all sources from all incoming messages into our routing tree,
-- since we have now seen them
slurpSourcesIntoTree :: [MSG.Message] -> RD.ID -> RD.Tree -> RD.Tree
slurpSourcesIntoTree incoming myID tree = L.foldl' folder tree sources
  where sources = fmap MSG.src incoming
        folder accTree source = RD.insert accTree myID source

-- | Wrapped `slurpSourcesIntoTree`
slurpSources :: Node -> Node
slurpSources n@Node{..} = n { nTree = slurpSourcesIntoTree nIncoming nNodeID nTree }

-- | Store all source metadata we see in incoming messages
slurpSourceInfosIntoMap :: [MSG.Message]
                        -> NodeInfos
                        -> NodeInfos
slurpSourceInfosIntoMap incoming infos = L.foldl' folder infos incoming
  where folder accMap mess = HM.insert (MSG.src mess) (MSG.srcInfo mess) accMap

-- | Wrapped `slurpSourceInfosIntoMap`.
slurpSourceInfos :: Node -> Node
slurpSourceInfos n@Node{..}
  = n { nNodeInfos = slurpSourceInfosIntoMap nIncoming nNodeInfos }

-- TODO: last seen, last sent

-- | From any FIND_NODE responses, get the node metadata from the results
slurpNewNodeInfosIntoMap :: [MSG.Message]
                         -> NodeInfos
                         -> NodeInfos
slurpNewNodeInfosIntoMap incoming infos
  = L.foldl' folder infos $ concat $ fmap MSG.getNodeInfos incoming
  where folder accMap info = HM.insert (fst info) (snd info) accMap

-- | Wrapped `slurpNewNodeInfos`.
slurpNewNodeInfos :: Node -> Node
slurpNewNodeInfos n@Node{..}
  = n { nNodeInfos = slurpNewNodeInfosIntoMap nIncoming nNodeInfos }

-- | From any FIND_NODE responses, place the resulting IDs into the tree
slurpNewNodeIDsIntoTree :: [MSG.Message] -> RD.ID -> RD.Tree -> RD.Tree
slurpNewNodeIDsIntoTree incoming myID tree
  = L.foldl' folder tree $ concat $ fmap MSG.getNodeInfos incoming
  where folder accTree info = RD.insert accTree myID (fst info)

-- | Wrapped `slurpNewNodeIDs`.
slurpNewNodeIDs :: Node -> Node
slurpNewNodeIDs n@Node{..}
  = n { nTree = slurpNewNodeIDsIntoTree nIncoming nNodeID nTree }

-- | Pull any new information from nodes we receiving messages from into
-- the respective information hashmaps
slurp :: Node -> Node
slurp = slurpNewNodeIDs . slurpNewNodeInfos . slurpSources . slurpSourceInfos


-- ||| Responding to FIND_NODE messages

-- | Response to a FIND_NODE message. Return the `k` closest
-- nodes we know about to a target, from our routing tree.
respondFN :: RD.Tree -- | Our routing tree
          -> NodeInfos -- | To look up metadata
          -> RD.ID -- | Our ID
          -> RD.IPInfo -- | Our IP info
          -> Clock.UTCTime -- | Right now
          -> MSG.Message -- | The message we've received
          -> Maybe MSG.Message -- | A FIND_NODE response, if message was FIND_NODE
respondFN tree infos myID myInfo now MSG.FindNode{..}
    = Just $ MSG.FindNodeR myID myInfo src results now mRound mID qID
  where sorter a b = compare (RD.nodeDistance target a)
                             (RD.nodeDistance target b)
        resultIDs = take C.k $ L.sortBy sorter $ RD.treeToList tree
        mapper nodeID = fmap ((,) nodeID) $ HM.lookup nodeID infos
        results = M.catMaybes $ fmap mapper resultIDs
respondFN _ _ _ _ _ _ = Nothing

-- slurp find values that we know the value for (stores must happen before this)

-- | Maps `respondFN`.
respondFindNodeMs :: [MSG.Message] -> RD.ID -> RD.IPInfo -> Clock.UTCTime
                  -> RD.Tree -> NodeInfos -> [MSG.Message]
respondFindNodeMs incoming myID myInfo now tree infos
  = M.catMaybes $ fmap (respondFN tree infos myID myInfo now) incoming

-- | Wraps `respondFindNodeMs`.
respondFindNodes :: Node -> Clock.UTCTime -> Node
respondFindNodes n@Node{..} now = n { nOutgoing = fnrs }
  where fnrs = respondFindNodeMs nIncoming nNodeID (nIP, nPort) now nTree nNodeInfos
