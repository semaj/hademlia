module Sim where
import           Message
import           Node
import           RoutingData

import qualified Data.HashMap.Lazy as HM
import           System.Random

data Sim = Sim { nodes :: HM.HashMap IPInfo Node
               , queue :: [Message]
               , gen :: StdGen
               }

numNodes = 5

start :: StdGen -> IO ()
start gen = putStrLn "Yeah, I simulate things"
