module Node where
import qualified Constants as C
import           Message
import           Query
import           RoutingData
import qualified Utils as U

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Data.Time.Clock

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
