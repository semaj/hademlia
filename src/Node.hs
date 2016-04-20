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

data QueryResult = FoundValue T.Text | FoundNodes [NodeInfo] | NotDone
                 deriving (Show, Eq)

data QueryMessage = QM { qmDest :: ID
                       , qmRound :: Int
                       , qmTarget :: ID
                       } deriving (Show)

data QueryMessageResponse = QMR { qmrSrc :: ID
                                , qmrRound :: Int
                                , qmrResults :: [ID]
                                } deriving (Show)

data QueryState = QueryState { heap :: H.MinHeap NodeHeapInfo
                             , queryID :: QueryID
                             , roundResps :: Int
                             , round :: Int
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
                                            , round = 0
                                            , desperate = False
                                              -- include entire tree in seen?
                                            , seen = HS.fromList $ fmap snd aClosest
                                            , qincoming = []
                                            , qoutgoing = []
                                            }
  where aClosest = take C.a $ L.sort $ fmap (\n -> (nodeDistance target n, n)) nodes
        aHeap = H.fromAscList $ fmap (\(dist, n) -> NHI False n dist) aClosest

kQueried :: H.MinHeap NodeHeapInfo -> Bool
kQueried heap = all (((==) True) . queried) $ H.take C.k heap

-- the node infos need to be parsed out and handled in the
-- nodes internal map to keep track of the IP, port
process :: QueryState -> QueryState
process QueryState{..} = QueryState{..}
  where roundResps = length $ filter (((==) round) . qmrRound) incoming
        newIDs = HS.difference (HS.fromList $ concat $ fmap qmrResults incoming) seen
        heap = HS.foldl' (\h n -> H.insert (NHI False n $ nodeDistance qtarget n) h)
          heap newIDs
        incoming = []

-- should I increment the round after this? I think so.
-- set desparate to true
shotgunK q = undefined

-- return the k closest we know about
terminate q = undefined

-- increment round, send to next a
continue q = undefined

-- findValue filter can occur before findNode starts.. filter
-- out incoming messages and decide whether to continue finding the node!!!

findNode :: QueryState -> QueryState
findNode q@QueryState{..}
  | qresult /= NotDone = q
  | kQueried heap && desperate = terminate q
  | roundResps == C.a && kQueried heap = shotgunK q
  | roundResps == C.a = continue q
  | roundResps /= C.a = q -- TODO: timeout messages
  | otherwise = error "wtf"
