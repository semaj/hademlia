module Node where
import qualified Constants as C
import           Message
import           Query
import           RoutingData
import qualified Utils as U

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Data.Time.Clock

data Node = Node { nPort :: Port
                 , nIP :: IP
                 , nNodeID :: ID
                 , nTree :: Tree
                 , nStore :: HM.HashMap ID T.Text
                 , nFindValueQueries :: HM.HashMap QueryID Query
                 , nFindNodeQueries :: HM.HashMap QueryID Query
                 , nIncoming :: [Message]
                 , nOutgoing :: [Message]
                 , nUserStores :: [(ID, T.Text)]
                 , nLastSeen :: HM.HashMap ID UTCTime
                 , nLastSent :: HM.HashMap ID Message
                 , nNodeInfos :: HM.HashMap ID IPInfo
                 }
