module Message where
import           Query
import           RoutingData

import           Data.Aeson
import qualified Data.Maybe as M
import           Data.Text
import           Data.Time.Clock
import           GHC.Generics


-- let's assume network is reliable right? we don't need an ack...
-- https://www.youtube.com/watch?v=JG2ESDGwHHY

-- | These messages are sent between nodes.
data Message
  = Store
  { src :: ID                          -- ^ Who sent it?
  , srcInfo :: IPInfo
  , key :: ID                          -- ^ Under what key are we storing the value?
  , value :: Text                      -- ^ What value are we storing?
  , dest :: ID                         -- ^ Who is it destined for?
  , sent :: UTCTime                    -- ^ When was it sent?
  , mID :: Text                        -- ^ Unique UUID for this message
  }
  | FindNode
  { src :: ID                          -- ^ Who sent it?
  , srcInfo :: IPInfo
  , dest :: ID                         -- ^ Who is it destined for?
  , target :: ID                       -- ^ Who are we trying to find?
  , sent :: UTCTime                    -- ^ When was it sent?
  , mRound :: Int                      -- ^ Which find-Round does it belong to?
  , mID :: Text                        -- ^ Unique UUID for this message
  , qID :: QueryID                     -- ^ Unique UUID for query it belongs to
  }
  | FindNodeR
  { src :: ID                          -- ^ Who sent it?
  , srcInfo :: IPInfo
  , dest :: ID                         -- ^ Who is it destined for?
  , results :: [NodeInfo]              -- ^ Who is close to the target?
  , sent :: UTCTime                    -- ^ When was it sent?
  , mRound :: Int                      -- ^ Which find-Round does it belong to?
  , mID :: Text                        -- ^ Unique UUID for this message
  , qID :: QueryID                     -- ^ Unique UUID for query it belongs to
  }
  | FindValue
  { src :: ID                          -- ^ Who sent it?
  , srcInfo :: IPInfo
  , dest :: ID                         -- ^ Who is it destined for
  , key :: ID                          -- ^ Under which key is the value we're trying to obtain?
  , sent :: UTCTime                    -- ^ When was it sent?
  , mID :: Text                        -- ^ Unique UUID for this message
  , qID :: QueryID                     -- ^ Unique UUID for query it belongs to
  }
  | FindValueR
  { src :: ID                          -- ^ Who sent it?
  , srcInfo :: IPInfo
  , dest :: ID                         -- ^ Who is it destined for?
  , key :: ID                          -- ^ What keyw as the value under?
  , value :: Text                      -- ^ What value did we have under that key?
  , sent :: UTCTime                    -- ^ When was it sent?
  , mID :: Text                        -- ^ Unique UUID for this message
  , qID :: QueryID                     -- ^ Unique UUID for query it belongs to
  }
  deriving (Show)

isFindValueR :: Message -> Bool
isFindValueR FindValueR{..} = True
isFindValueR _ = False

getNodeInfos :: Message -> [NodeInfo]
getNodeInfos FindNodeR{..} = results
getNodeInfos _ = []

cleanFindValueR :: Message -> Maybe (QueryID, ID, Text)
cleanFindValueR FindValueR{..} = Just (qID, key, value)
cleanFindValueR _ = Nothing

toQueryMessageResponse :: Message -> Maybe (QueryID, QueryMessageResponse)
toQueryMessageResponse FindNodeR{..} = Just (qID, QMR src mRound $ fmap fst results)
toQueryMessageResponse _ = Nothing

bulkToQueryMessageResponse :: [Message] -> [(QueryID, QueryMessageResponse)]
bulkToQueryMessageResponse = M.catMaybes . fmap toQueryMessageResponse

findNodeFromQM :: ID -> IPInfo -> UTCTime -> QueryID -> Text -> QueryMessage -> Message
findNodeFromQM myID ip now qID messageID QM{..} = FindNode myID ip qmDest qmTarget now qmRound messageID qID

findValueFromQM :: ID -> IPInfo -> UTCTime -> QueryID -> Text -> QueryMessage -> Message
findValueFromQM myID ip now qID messageID QM{..} = FindValue myID ip qmDest qmTarget now messageID qID

deserialize :: String -> Message
deserialize s = undefined

serialize :: Message -> String
serialize m = undefined
