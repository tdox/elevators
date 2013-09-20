module Elevators.Building where

-- * base
--import Debug.Trace (trace)

--------------------------------------------------------------------------------

type Floor   = Int
type Time    = Float
type RiderId = Int

data Rider = MkRider { rId    :: RiderId
                     , rStart :: Floor
                     , rEnd   :: Floor
                     } deriving (Eq, Ord, Show)

data Goal = PickUpG  Rider 
          | DropOffG Rider
--        | GoTo Floor
            deriving (Show)

type ElevatorId = Int

data Elevator = MkElevator { eId        :: ElevatorId
                           , eCurrFloor :: Floor
                           , eGoals     :: [Goal]
                           } deriving Show


data Direction = GoingUp | GoingDown | Waiting deriving (Eq, Show)

--                                start  end
data Order = MkOrder Time RiderId Floor Floor deriving Show

data Building =
  MkBuilding { bNFloors           :: Int
             , bElevators        :: [Elevator]
             , bOrders           :: [Order]
             , bPendingGoals     :: [Goal]
             , bCompletedGoals   :: [Goal]
             , bSimPeriod        :: Float -- ^ seconds to wait between each step
             , bElapsedStepTime  :: Float -- ^ time since last step
             , bElapsedTotalTime :: Float -- ^ time since t=0
             } deriving Show

data StartOrEndFloor = StartFloor | EndFloor deriving (Eq, Show)

--------------------------------------------------------------------------------
                                    
instance Eq Goal where
  (PickUpG r1)  == (PickUpG  r2) = (rStart r1) == (rStart r2)
  (PickUpG r1)  == (DropOffG r2) = (rStart r1) == (rEnd   r2)
  (DropOffG r1) == (PickUpG  r2) = (rEnd   r1) == (rStart r2)
  (DropOffG r1) == (DropOffG r2) = (rEnd   r1) == (rEnd   r2)
  
instance Ord Goal where  
  compare (PickUpG r1)  (PickUpG  r2) = compare (rStart r1) (rStart r2)
  compare (PickUpG r1)  (DropOffG r2) = compare (rStart r1) (rEnd   r2)
  compare (DropOffG r1) (PickUpG  r2) = compare (rEnd   r1) (rStart r2)
  compare (DropOffG r1) (DropOffG r2) = compare (rEnd   r1) (rEnd   r2)

--------------------------------------------------------------------------------

isPickUpGoal :: Goal -> Bool
isPickUpGoal (PickUpG _) = True
isPickUpGoal _           = False

isDropOffGoal :: Goal -> Bool
isDropOffGoal (DropOffG _) = True
isDropOffGoal _            = False

mkGoal :: Order -> Goal
mkGoal (MkOrder _ iD start end) = PickUpG (MkRider iD start end)

goalRider :: Goal -> Rider
goalRider (PickUpG  r) = r
goalRider (DropOffG r) = r
-- goalRider _          = error "goalRider: should not be called"

getElevatorPickUpGoals :: Elevator -> [Goal]
getElevatorPickUpGoals el = filter isPickUpGoal $ eGoals el

getBuildingPickUpGoals :: Building -> [Goal]
getBuildingPickUpGoals b =
  concatMap getElevatorPickUpGoals (bElevators b)
  ++ (filter isPickUpGoal $ bPendingGoals b)



mkRidersWaitingOnFloorLists :: Building -> [[Rider]]
mkRidersWaitingOnFloorLists b =
  mkRidersFloorLists (bNFloors b) StartFloor $ getPickUpGoals b

{-
  mkRidersFloorLists (bNFloors b) StartFloor (concatMap getElevatorPickUpGoals
                                                       (bElevators b)
                                             )
-}


getPickUpGoals :: Building -> [Goal]
getPickUpGoals b = pickUpGoalsOnEls ++ pendingPickUpGoals
  where          
    pickUpGoalsOnEls = concatMap getElevatorPickUpGoals (bElevators b) :: [Goal]
    pendingPickUpGoals = bPendingGoals b :: [Goal]

  
mkRidersArrivedOnFloorLists :: Building -> [[Rider]]
mkRidersArrivedOnFloorLists b =
  mkRidersFloorLists (bNFloors b) EndFloor (bCompletedGoals b)
    
mkRidersFloorLists :: Int -> StartOrEndFloor -> [Goal] -> [[Rider]]
mkRidersFloorLists nFloors sOrE gs =
  foldr (putInList sOrE) (replicate nFloors []) $ map goalRider gs


putInList :: StartOrEndFloor -> Rider -> [[Rider]] -> [[Rider]]
putInList sOrE r@(MkRider _ start end) ridersOnFloors
  | start > length ridersOnFloors = error "putInList: start > building height"
  | end   > length ridersOnFloors = error "putInList: end > building height"
  | otherwise = below ++ ((r:at0) : above)
  where
    pos = if sOrE == StartFloor then start else end
    (below, at0 : above) = splitAt (pos-1) ridersOnFloors


getRidersOnFloor :: Int -> [[Rider]] -> [Rider]


{-
getRidersOnFloor iFloor rs | trace ("getRidersOnFloor: iFloor: " ++ show iFloor
                                    ++ "\nrs: " ++ show rs
                                   )
                                  False = undefined
-}


getRidersOnFloor iFloor ridersOnFloors = ridersOnFloors !! (iFloor - 1)

getRidersInElevator :: Elevator -> [Rider]
getRidersInElevator el = map goalRider $ filter isDropOffGoal $ eGoals el

getRiderIdsInElevator :: Elevator -> [RiderId]
getRiderIdsInElevator el = map rId $ getRidersInElevator el


{-
(!!.) :: [a] -> Int -> a
l !!. i | length l <= i = error $ "(!!.): i: " ++ show i ++ "length: "
                                  ++ show (length l)
        | otherwise = l !! i
-}