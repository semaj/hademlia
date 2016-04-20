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
                             , respsRemain :: Int
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
                                            , respsRemain = C.a
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

-- the node infos need to be parsed out and handled in the
-- nodes internal map to keep track of the IP, port
process :: QueryState -> QueryState
process QueryState{..}
  | respsRemain < 0 = error "process wtf"
  | otherwise = QueryState{..}
  where respsRemain = respsRemain - (length $ filter (((==) qround) . qmrRound) qincoming)
        newIDs = HS.difference (HS.fromList $ concat $ fmap qmrResults qincoming) seen
        heap = HS.foldl' (\h n -> H.insert (NHI False n $ nodeDistance qtarget n) h)
          heap newIDs
        qincoming = []

toUnqueried :: Int -> ID -> Int -> H.MinHeap NodeHeapInfo -> [QueryMessage]
toUnqueried only target round heap = new
  where filtered = filter (not . queried) $ H.take only heap
        new = fmap ((QM round target) . nID) filtered

terminate :: H.MinHeap NodeHeapInfo -> QueryResult
terminate heap = FoundNodes $ fmap nID $ H.take C.k heap

-- findValue filter can occur before findNode starts.. filter
-- out incoming messages and decide whether to continue finding the node!!!

-- this is confusing. refactor or die
findNode :: QueryState -> QueryState
findNode q@QueryState{..}
  | qresult /= NotDone = q
  | desperate = desperation q
  | respsRemain == 0 && kQueried heap = panic
  | respsRemain == 0 = carryOn
  | respsRemain > 0 = q -- TODO: timeout messages
  | otherwise = error "wtf"
  where toMaxK = toUnqueried C.k qtarget qround heap
        toMaxA = toUnqueried C.a qtarget qround heap
        plusRound = q { qround = qround + 1 }
        panic = plusRound { desperate = True, respsRemain = length toMaxK
                          , qoutgoing = qoutgoing ++ toMaxK }
        carryOn = plusRound { qoutgoing = qoutgoing ++ toMaxA
                            , respsRemain = length toMaxA }

desperation :: QueryState -> QueryState
desperation q@QueryState{..}
  | respsRemain == 0 && kQueried heap = q { qresult = terminate heap }
  | respsRemain > 0 && kQueried heap = q
  | respsRemain > 0 && (not $ kQueried heap) = calmDown
  | respsRemain == 0 && (not $ kQueried heap) = error "desperation wtf"
    where toMaxA = toUnqueried C.a qtarget qround heap
          calmDown = q { qround = qround + 1, desperate = False
                       , respsRemain = length toMaxA
                       , qoutgoing = qoutgoing ++ toMaxA }
