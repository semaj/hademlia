module Main where
import qualified Real
import qualified Sim
import           System.Environment
import           System.IO
import           Utils

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  args <- getArgs
  let simOrReal = get args 0
      port = get args 1
      bootstrap = get args 2
  choosePath simOrReal port bootstrap

choosePath :: Maybe String -> Maybe String -> Maybe String -> IO ()
choosePath Nothing _ _ = error "You must provide a sim/real parameter"
choosePath (Just "sim") _ _ = Sim.start
choosePath _ Nothing _ = error "You must provide a port for this node"
choosePath (Just "real") (Just port) bootstrap = Real.start port bootstrap
choosePath (Just _) _ _ = error "First arg must be real or sim"
