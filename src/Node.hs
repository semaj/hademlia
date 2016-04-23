module Node where
import qualified Constants as C
import           Message
import           RoutingData
import qualified Utils as U

import qualified Data.HashMap.Strict as HM
import qualified Data.Heap as H
import qualified Data.List as L
import qualified Data.Text as T
import           Data.Time.Clock
import           Network.Socket


type NodeHeap = H.MinHeap NodeHeapInfo

data Node = Node { port :: Port
                 , ip :: IP
                 , nodeID :: ID
                 , tree :: Tree
                 , store :: HM.HashMap ID T.Text
                 , qstates :: HM.HashMap QueryID Query
                 , incoming :: [Message]
                 , outgoing :: [Message]
                 , userStores :: [(ID, T.Text)]
                 , lastTime :: HM.HashMap ID UTCTime
                 , lastSent :: HM.HashMap ID Message
                 , idToInfo :: HM.HashMap ID IPInfo
                 }

data NodeHeapInfo = NHI { nhiQueried :: Bool
                        , nhiNodeID :: ID
                        , nhiDistance :: Int
                        } deriving (Show)

instance Eq NodeHeapInfo where
  x == y = (nhiDistance x) == (nhiDistance y)

instance Ord NodeHeapInfo where
  x <= y = (nhiDistance x) <= (nhiDistance y)

data QueryState = FoundNodes [ID] | Recursing | Desperate
                 deriving (Show, Eq)

data QueryRound = QR { qrStillNeed :: Int
                     , qrRound :: Int
                     } deriving (Show, Eq)

data QueryMessage = QM { qmRound :: Int
                       , qmTarget :: ID
                       , qmDest :: ID
                       } deriving (Show, Eq)

data QueryMessageResponse = QMR { qmrSrc :: ID
                                , qmrRound :: Int
                                , qmrResults :: [ID]
                                } deriving (Show, Eq)

data Query = Query { qHeap :: NodeHeap
                   , qID :: QueryID
                   , qRound :: QueryRound
                   , qTarget :: ID
                   , qState :: QueryState
                   , qOutgoing :: [QueryMessage]
                   , qIncoming :: [QueryMessageResponse]
                   } deriving (Show, Eq)

startFindNode :: ID -> [ID] -> QueryID -> Query
startFindNode target nodes qid = Query { qHeap = aHeap
                                       , qID = qid
                                       , qTarget = target
                                       , qState = Recursing
                                       , qRound = QR C.a 0
                                         -- include entire tree in seen?
                                       , qIncoming = []
                                       , qOutgoing = []
                                       }
  where aClosest = take C.a $ L.sort $ fmap (\n -> (nodeDistance target n, n)) nodes
        aHeap = H.fromAscList $ fmap (\(dist, n) -> NHI False n dist) aClosest

kQueried :: NodeHeap -> Bool
kQueried heap = all nhiQueried $ H.take C.k heap

relevantQMRs :: [QueryMessageResponse] -> Int -> Int
relevantQMRs qmrs round = length $ filter (U.eq round) $ fmap qmrRound qmrs

modRound :: [QueryMessageResponse] -> QueryRound -> QueryRound
modRound qmrs q@QR{..} = q { qrStillNeed = qrStillNeed - relevantQMRs qmrs qrRound }

queryModRound :: Query -> Query
queryModRound q@Query{..} = q { qRound = modRound qIncoming qRound }

roundOver :: [QueryMessageResponse]    -- ^ Messages we've received
          -> QueryRound                -- ^ Current round
          -> Bool                      -- ^ Is the round over?
roundOver qmrs QR{..}
  | remaining < 0 = error "roundOver wtf"
  | otherwise = (0 == remaining)
  where remaining = qrStillNeed - (length $ filter (U.eq qrRound) $ fmap qmrRound qmrs)

nextRound :: Int                       -- ^ How many messages we need next round
          -> QueryRound                -- ^ Our current round
          -> QueryRound                -- ^ Updated round
nextRound newStillNeed qr@QR{..} = qr { qrRound = qrRound + 1
                                      , qrStillNeed = newStillNeed
                                      }

heapInsert :: ID -> NodeHeap -> ID -> NodeHeap
heapInsert target heap nID = H.insert (NHI False nID $ nodeDistance target nID) heap

bulkHeapInsert :: ID           -- ^ Target
               -> [ID]         -- ^ Discovered nodes (we may have seen some of these already)
               -> NodeHeap     -- ^ The current heap
               -> NodeHeap     -- ^ New heap, with the new IDs we havne't seen
bulkHeapInsert target ids currentHeap = L.foldl' (heapInsert target) currentHeap newIDs
  where seen = fmap nhiNodeID $ H.toList currentHeap
        newIDs = L.nub ids L.\\ seen

-- Actual IP info is handled outside of querying
heapifyIncomingIDs :: Query -> Query
heapifyIncomingIDs q@Query{..} = q { qHeap = bulkHeapInsert qTarget ids qHeap
                                   , qIncoming = [] }
  where ids = concat $ fmap qmrResults qIncoming

toUnqueried :: Int              -- ^ Max messages to send
            -> ID               -- ^ Target ID
            -> Int              -- ^ Current round
            -> NodeHeap         -- ^ Current heap
            -> [QueryMessage]   -- ^ List of max `only` messages to send out
toUnqueried only target round heap = new
  where filtered = filter (not . nhiQueried) (H.take only heap)
        new = fmap ((QM round target) . nhiNodeID) filtered

terminate :: NodeHeap -> QueryState
terminate heap = FoundNodes $ fmap nhiNodeID $ H.take C.k heap

-- findValue filter can occur before findNode starts.. filter
-- out incoming messages and decide whether to continue finding the node!!!

shouldDespair :: QueryRound -> NodeHeap -> Bool
shouldDespair QR{..} nodeHeap = qrStillNeed == 0 && kQueried nodeHeap

despair :: Query -> Query
despair q@Query{..} = q { qState = Desperate
                        , qRound = nextRound expect qRound
                        , qOutgoing = qOutgoing ++ outgoingToClosestK }
  where outgoingToClosestK = toUnqueried C.k qTarget (qrRound qRound) qHeap
        expect = length outgoingToClosestK

stepRecurse :: Query -> Query
stepRecurse q@Query{..} = q { qState = Recursing
                            , qRound = nextRound expect qRound
                            , qOutgoing = qOutgoing ++ outgoingToClosestA }
  where outgoingToClosestA = toUnqueried C.a qTarget (qrRound qRound) qHeap
        expect = length outgoingToClosestA

findNode :: Query -> Query
findNode = switchState . heapifyIncomingIDs . queryModRound

hasFoundNodes :: QueryState -> Bool
hasFoundNodes (FoundNodes _) = True
hasFoundNodes _ = False

switchState :: Query -> Query
switchState q@Query{..}
  | hasFoundNodes qState = q
  | qState == Desperate = desperation q
  | otherwise = recurse q

-- todo: timeout messages if it's been a while
recurse :: Query -> Query
recurse q@Query{..}
  | shouldDespair qRound qHeap = despair q
  | roundOver qIncoming qRound = stepRecurse q
  | otherwise = q

desperation :: Query -> Query
desperation q@Query{..}
  | shouldDespair qRound qHeap = q { qState = terminate qHeap }
  | roundOver qIncoming qRound && (not $ kQueried qHeap) = stepRecurse q
  | otherwise = q
