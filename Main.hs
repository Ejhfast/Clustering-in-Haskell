module Main (main) where
import Cluster
import System( getArgs )

main = do
	args <- getArgs
	runOnFile (args !! 0) (read (args !! 1) :: Int)
