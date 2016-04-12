module Message where
import RoutingData

import Data.Aeson
import Data.Text
import GHC.Generics
import Data.Time.Clock


-- let's assume network is reliable right? we don't need an ack...
-- https://www.youtube.com/watch?v=JG2ESDGwHHY

-- | These messages are sent between nodes.
data Message
  = Store
  { src :: ID                          -- ^ Who sent it?
  , key :: ID                          -- ^ Under what key are we storing the value?
  , value :: Text                      -- ^ What value are we storing?
  , dest :: ID                         -- ^ Who is it destined for?
  , sent :: UTCTime                    -- ^ When was it sent?
  , mID :: Text                        -- ^ Unique UUID for this message
  }
  | FindNode
  { src :: ID                          -- ^ Who sent it?
  , dest :: ID                         -- ^ Who is it destined for?
  , target :: ID                       -- ^ Who are we trying to find?
  , sent :: UTCTime                    -- ^ When was it sent?
  , mID :: Text                        -- ^ Unique UUID for this message
  }
  | FindNodeR
  { src :: ID                          -- ^ Who sent it?
  , dest :: ID                         -- ^ Who is it destined for?
  , results :: [NodeInfo]              -- ^ Who is close to the target?
  , sent :: UTCTime                    -- ^ When was it sent?
  , mID :: Text                        -- ^ Unique UUID for this message
  }
  | FindValue
  { src :: ID                          -- ^ Who sent it?
  , dest :: ID                         -- ^ Who is it destined for
  , key :: ID                          -- ^ Under which key is the value we're trying to obtain?
  , sent :: UTCTime                    -- ^ When was it sent?
  , mID :: Text                        -- ^ Unique UUID for this message
  }
  | FindValueR
  { src :: ID                          -- ^ Who sent it?
  , dest :: ID                         -- ^ Who is it destined for?
  , value :: Text                      -- ^ What value did we have under that key?
  , sent :: UTCTime                    -- ^ When was it sent?
  , mID :: Text                        -- ^ Unique UUID for this message
  }
  deriving (Show)

deserialize :: String -> Message
deserialize s = undefined

serialize :: Message -> String
serialize m = undefined
