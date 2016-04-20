module Node where
import qualified Constants as C
import           Message
import           RoutingData

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Heap as H
import qualified Data.List as L
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

data NodeHeapInfo = NHI { queried :: Bool
                        , nID :: ID
                        , distance :: Int
                        } deriving (Show)

instance Eq NodeHeapInfo where
  x == y = (distance x) == (distance y)

instance Ord NodeHeapInfo where
  x <= y = (distance x) <= (distance y)

data QueryResult = FoundNodes [ID] | NotDone
                 deriving (Show, Eq)

data QueryMessage = QM { qmRound :: Int
                       , qmTarget :: ID
                       , qmDest :: ID
                       } deriving (Show)

data QueryMessageResponse = QMR { qmrSrc :: ID
                                , qmrRound :: Int
                                , qmrResults :: [ID]
                                } deriving (Show)

data QueryState = QueryState { heap :: H.MinHeap NodeHeapInfo
                             , queryID :: QueryID
                             , roundResps :: Int
                             , qround :: Int
                             , qtarget :: ID
                             , qresult :: QueryResult
                             , desperate :: Bool
                             , seen :: HS.HashSet ID
                             , qoutgoing :: [QueryMessage]
                             , qincoming :: [QueryMessageResponse]
                             } deriving (Show)

startFindNode :: ID -> [ID] -> QueryID -> QueryState
startFindNode target nodes qid = QueryState { heap = aHeap
                                            , queryID = qid
                                            , qtarget = target
                                            , qresult = NotDone
                                            , roundResps = 0
                                            , qround = 0
                                            , desperate = False
                                              -- include entire tree in seen?
                                            , seen = HS.fromList $ fmap snd aClosest
                                            , qincoming = []
                                            , qoutgoing = []
                                            }
  where aClosest = take C.a $ L.sort $ fmap (\n -> (nodeDistance target n, n)) nodes
        aHeap = H.fromAscList $ fmap (\(dist, n) -> NHI False n dist) aClosest

kQueried :: H.MinHeap NodeHeapInfo -> Bool
kQueried heap = all queried $ H.take C.k heap

unqueriedFromK :: H.MinHeap NodeHeapInfo -> [ID]
unqueriedFromK heap = fmap nID $ filter (not . queried) $ H.take C.k heap

-- the node infos need to be parsed out and handled in the
-- nodes internal map to keep track of the IP, port
process :: QueryState -> QueryState
process QueryState{..} = QueryState{..}
  where roundResps = length $ filter (((==) qround) . qmrRound) qincoming
        newIDs = HS.difference (HS.fromList $ concat $ fmap qmrResults qincoming) seen
        heap = HS.foldl' (\h n -> H.insert (NHI False n $ nodeDistance qtarget n) h)
          heap newIDs
        qincoming = []

shotgunK :: QueryState -> QueryState
shotgunK QueryState{..} = QueryState{..}
  where qround = qround + 1
        desperate = True
        qoutgoing = qoutgoing ++ (fmap (QM qround qtarget) $ unqueriedFromK heap)

terminate :: QueryState -> QueryState
terminate QueryState{..} = QueryState{..}
  where qresult = FoundNodes $ fmap nID $ H.take C.k heap

continue :: QueryState -> QueryState
continue QueryState{..} = QueryState{..}
  where qround = qround + 1
        qoutgoing = qoutgoing ++ (fmap (QM qround qtarget) $ take C.a $ unqueriedFromK heap)

-- findValue filter can occur before findNode starts.. filter
-- out incoming messages and decide whether to continue finding the node!!!

-- this is confusing. refactor or die
findNode :: QueryState -> QueryState
findNode q@QueryState{..}
  | qresult /= NotDone = q
  | roundResps == C.k && kQueried heap && desperate = terminate q
  | roundResps /= C.k && kQueried heap && desperate = continue q
  | not $ kQueried heap && desperate = q { desperate = False, qround = qround + 1 }
  | roundResps == C.a && kQueried heap = shotgunK q
  | roundResps == C.a = continue q
  | roundResps /= C.a = q -- TODO: timeout messages
  | otherwise = error "wtf"
