
-- * base
import Control.Monad      (when)
-- import Debug.Trace        (trace)
import System.Environment (getArgs, getProgName)

-- * gloss
import Graphics.Gloss                         (Display(InWindow), white)
import Graphics.Gloss.Interface.Pure.Simulate (ViewPort, simulate)

-- * elevators
import Elevators.Building
import Elevators.Graphics
import Elevators.Logic

--------------------------------------------------------------------------------
nFloors    :: Int
nFloors    = 14

nElevators :: Int
nElevators = 2
--------------------------------------------------------------------------------

main :: IO ()
main = do
  
  args     <- getArgs
  progName <- getProgName
  when (null args) (error $ "usage: " ++ progName ++ " filename")
  orders <- readOrders (head args)
  putStrLn "orders:\n"
  mapM_ (putStrLn . show) orders
  
  let 
    els = map (\i ->  MkElevator i 1 []) [1 .. nElevators]
    b0 = MkBuilding nFloors els orders [] [] 0.1 0 0 :: Building
      
  simulate (InWindow "Building"
                    (windowWidthInt, windowHtInt)
                    (windowWidthInt `div` 2, windowHtInt `div` 2)
           )
           white
           4
           b0
           drawBuilding
           simulateBuilding


simulateBuilding :: ViewPort -> Float -> Building -> Building
simulateBuilding _ time b0
  	-- If enough time has passed then it's time to step the world.
  
--        | trace ("simulateBuilding: \n" ++ show b0 ++ "\n") False = undefined
                          
	| bElapsedStepTime b0 >= (bSimPeriod b0)
	= let b1 = stepBuilding b0
	  in  b1 { bElapsedStepTime = 0
                 , bElapsedTotalTime = bElapsedTotalTime b0 + time}
	
	-- Wait some more.
	| otherwise
	= b0 { bElapsedStepTime  = bElapsedStepTime  b0 + time
             , bElapsedTotalTime = bElapsedTotalTime b0 + time
             }

