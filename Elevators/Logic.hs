module Elevators.Logic where

{-
Rules:
 * An elevator with a rider shall not change directions.
 * Everyone who requests a ride will eventually get a ride.
-}

-- * base
import Control.Monad      ((>=>), foldM)
import Data.List          (insert, partition, sortBy)
import Data.Maybe     (catMaybes)
-- import Debug.Trace    (trace)

-- * mtl
import Control.Monad.State.Lazy (StateT, get, put)

-- * transformers
import Control.Monad.IO.Class (liftIO)

import Elevators.Building

  
--------------------------------------------------------------------------------

direction :: Floor -> Floor -> Direction
direction from to =
  case compare from to of
    LT -> GoingUp
    GT -> GoingDown
    _  -> Waiting
    
riderDir :: Rider -> Direction
riderDir (MkRider _ start end) = direction start end
  
goalFloor :: Goal -> Floor  
goalFloor (PickUpG  r) = rStart r
goalFloor (DropOffG r) = rEnd   r
  
elDir :: Elevator -> Direction
elDir (MkElevator _ _  []) = Waiting
elDir (MkElevator _ cf gs) = direction cf $ goalFloor $ head gs
                    
acceptNewGoal2 :: Goal -> Elevator -> Elevator
acceptNewGoal2  g el@(MkElevator _ _ gs) = el{eGoals = newGoals}
  where
    dir = case elDir el of
      Waiting -> riderDir $ goalRider g
      elevDir -> elevDir

    newGoals = case dir of
      GoingUp   -> insert g gs
      GoingDown -> reverse $ insert g $ reverse gs
      _         -> error "acceptNewGoal2: should not be here"
  

pickUpRider2 :: Elevator -> Elevator
pickUpRider2 el@(MkElevator _ _ (g:gs)) =
  case g of 
    DropOffG _ -> error "pickUpRider: head is DropOffG"
    PickUpG r -> acceptNewGoal2 (DropOffG r) el{eGoals = gs}

pickUpRider2 _ = error "pickUpRider2: no goals"

dropOffRider2 :: Elevator -> Elevator

dropOffRider2 el@(MkElevator _ _ (g:gs)) =
  case g of
    PickUpG  _ -> error "dropOffRider: head is PickUpG"
    DropOffG _ -> el{eGoals = gs}

dropOffRider2 _ = error "dropOffRider2: no goals"



moveUp :: Elevator -> Elevator
moveUp (MkElevator iD cf gs) = MkElevator iD (cf + 1) gs

moveDown :: Elevator -> Elevator
moveDown (MkElevator iD cf gs) = MkElevator iD (cf - 1) gs

wait :: Elevator -> Elevator
wait = id


elTick0 :: Elevator -> (Maybe Goal, Elevator)
elTick0 el@(MkElevator _ _ []) = (Nothing, el)

elTick0 e@(MkElevator _ f (g:_)) =
  if f == goalFloor g
  then case g of
    PickUpG  _ -> (Nothing, pickUpRider2 e)
    DropOffG _ -> (Just g, dropOffRider2 e)
  else case elDir e of
    GoingUp   -> (Nothing, moveUp e)
    GoingDown -> (Nothing, moveDown e)
    Waiting   -> (Nothing, wait e)
    
    
elTick :: Maybe Goal ->  Elevator -> (Maybe Goal, Elevator)
elTick mNewG e0 = elTick0 $ maybe e0 (\g -> acceptNewGoal2 g e0) mNewG

continue1 :: Elevator -> (Maybe Goal, Elevator)
continue1 e = elTick Nothing e

isBetweenInclusive :: Ord a => a -> a -> a -> Bool
isBetweenInclusive y x z = (x <= y && y <= z) || (x >= y && y >= z)
                                
readOrders :: String -> IO [Order]
readOrders fName = do
  str <- readFile fName
  let ls = lines str
  return $! map getOrder ls
 
getOrder :: String -> Order
getOrder str = MkOrder (read t) (read iD) (read s) (read e)
 where
   [t, iD, s, e] = words str

orderTime :: Order -> Time
orderTime (MkOrder t _ _ _) = t

getNowOrders :: [Order] -> ([Order], [Order])
getNowOrders [] = ([],[])
getNowOrders os = partition (\o -> now == orderTime o) os
  where
    now = orderTime $ head os


stepBuilding :: Building -> Building

{-
stepBuilding b0 |
  trace (   "stepBuilding"
         ++ "\n bElapsedTotalTime: " ++ show (bElapsedTotalTime b0)
        ) False = undefined
-}



stepBuilding b0 = b2
  where

    b1 = distributeGoalsToEls b0
    els0 = bElevators b1
        
    mGsAndEls = map continue1 els0 :: [(Maybe Goal, Elevator)]
    completedGs = catMaybes $ map fst mGsAndEls
    completedGs1 = completedGs ++ bCompletedGoals b0
    els1 = map snd mGsAndEls
    b2 =  b1{bElevators = els1, bCompletedGoals = completedGs1}


distributeGoalsToEls :: Building -> Building
distributeGoalsToEls b0 = b1
  where
    os   = bOrders b0
    els0 = bElevators b0
    pendingGs0 = bPendingGoals b0
    elapsedT   = bElapsedTotalTime b0

    b1 = if   (not (null pendingGs0))
           || ((not (null os)) && elapsedT >= orderTime (head os))
         then
           let (os1, os2) = getNowOrders os
               gs = map mkGoal os1 ++ pendingGs0
--               gs = trace ("gs: " ++ show gs_tr ++ "\n") gs_tr

               (pendingGs1, els1) = foldr (\g -> processPickUpGoal g)
                                          ([], els0)
                                          gs

           in b0{bOrders = os2, bElevators = els1, bPendingGoals = pendingGs1}
         else b0

--    os = trace ("os: " ++ show os_tr ++ "\n") os_tr
--    elapsedT = trace ("elapsedT: " ++ show elapsedT_tr) elapsedT_tr



processPickUpGoal :: Goal -> ([Goal], [Elevator])
            -> ([Goal], [Elevator])

processPickUpGoal (DropOffG _) _ = error "processPickUpGoal: must be pickup"

{-
processPickUpGoal g (_, els) | trace ("processGoal:" 
                           ++ "\n g: " ++ show g
                           ++ "\n els: " ++ show els
                          ) False = undefined
-}

processPickUpGoal g (pendingGs0, els0) = (pendingGs1, els1)
  where
    (passingByEls, otherEls1) = partition (elIsPassingBy g) els0
    (idleEls, otherEls2)      = partition elIsIdle els0
    (compatEls, otherEls3)    = partition (elHasCompatiblePickUpGoal g) els0

    (pendingGs1, els1) = if not $ null compatEls
       then (pendingGs0, addGoal g compatEls otherEls3)
       else if not $ null passingByEls
            then (pendingGs0, addGoal g passingByEls otherEls1)
            else
              if not $ null idleEls
              then (pendingGs0, addGoal g idleEls otherEls2)
              else (g:pendingGs0, els0)

--    passingByEls = trace ("passingByEls: " ++ show passingByEls_tr ++ "\n") passingByEls_tr



getElWithCompatibleGoal :: Goal -> [Elevator] -> Maybe Elevator

getElWithCompatibleGoal (DropOffG _) _ = error "getElsWithSamePickUpStart"
getElWithCompatibleGoal _ [] = Nothing

getElWithCompatibleGoal g els =
  if null compatEls
  then Nothing
  else Just $ head compatEls
  where
    compatEls = filter (elHasCompatiblePickUpGoal g) els

    

elHasCompatiblePickUpGoal :: Goal -> Elevator -> Bool
elHasCompatiblePickUpGoal (DropOffG _) _ = error "elHasCompatiblePickUpGoal"

elHasCompatiblePickUpGoal g el
  | null $ elGoals = False
  | otherwise = not $ null $ filter (isCompatiblePickUpGoal g) elGoals
  where
    elGoals = eGoals el


addGoal :: Goal -> [Elevator] -> [Elevator] -> [Elevator]

addGoal g okEls otherEls1 = els2
  where
    (goalEl0, otherEls2) = findClosestElToGoal g okEls
    goalEl1 = acceptNewGoal2 g goalEl0
    els1 = goalEl1 : (otherEls1 ++ otherEls2)
    els2 = sortBy (\ x y -> compare (eId x) (eId y)) els1



compatiblePickUpGoals :: Goal -> [Goal] -> [Goal]
compatiblePickUpGoals g@(PickUpG _) gs = filter (isCompatiblePickUpGoal g) gs
compatiblePickUpGoals _ _ = error "compatiblePickUpGoals: not pickup goal"


isCompatiblePickUpGoal :: Goal -> Goal -> Bool

isCompatiblePickUpGoal (PickUpG _) (DropOffG _) = False

isCompatiblePickUpGoal (PickUpG r1) (PickUpG r2) =
  start1 == start2 && riderDir r1 == riderDir r2
  where
    start1 = rStart r1
    start2 = rStart r2

isCompatiblePickUpGoal (DropOffG _) _ =
  error "isCompatiblePickUpGoal: not pickup goal"
    

-- | An elevator can accept an order if elevator's queue is empty,
--   of if the rider is going in the same direction as the elevator
--   and rider's start floor is between the elevators current flor 
--   and floor of elevator's last goal

elCanAcceptGoal :: Goal -> Elevator -> Bool
elCanAcceptGoal g el = 
  elIsIdle el || elIsPassingBy g el
              
elIsPassingBy :: Goal -> Elevator -> Bool

{-
elIsPassingBy (PickUpG r) el
  | trace ("elIsPassingby:"
           ++ "\n el: " ++ show el
           ++ "\n r: " ++ show r
           ) False = undefined   
   

elIsPassingBy g el = rlst
  where
    rlst_tr = elIsPassingBy_tr g el

    rlst = trace ("  rlst: " ++ show rlst_tr) rlst_tr
-}

elIsPassingBy (PickUpG r@(MkRider _ start _)) el@(MkElevator _ currF gs) =
  if null gs
  then start == currF
  else
    isBetweenInclusive start currF lastFloor
    && (elDirection == Waiting ||  elDirection == riderDir r)
  where lastFloor = goalFloor $ last gs
        elDirection = elDir el

elIsPassingBy _ _ = error "Logic.elIsPassingBy: not yet implemented"

elIsIdle :: Elevator -> Bool
elIsIdle el = null $ eGoals el

findClosestElToGoal :: Goal -> [Elevator] -> (Elevator, [Elevator])
findClosestElToGoal g els0
  | null els0 = error "findClosestElToGoal: null elevators"
  | otherwise = (closestEl, otherEls)
  where
    elsAndDist1  = zip els0 $ map (elDistanceFromGoal g) els0
                  :: [(Elevator, Int)]
                      
    elsAndDist2 = sortBy (\ x y -> compare (snd x) (snd y)) elsAndDist1
    closestEl : otherEls = map fst elsAndDist2

elDistanceFromGoal :: Goal -> Elevator -> Int
elDistanceFromGoal (PickUpG r) el = abs (rStart r - eCurrFloor el)
elDistanceFromGoal _ _ = error "elDistanceFromGoal: not yet implemented"
  
                         
-----------------------------------------------------------------------

{-  
elTickIO :: Maybe Goal -> Elevator -> IO Elevator
elTickIO mg e0 = do
  let (_, e1) = elTick mg e0
  putStrLn $ show e1
  return e1
  
concatM :: (Monad m) => [a -> m a] -> (a -> m a)
concatM fs = foldr (>=>) return fs

continueIO :: Int -> Elevator -> IO Elevator
continueIO nTimeSteps e = concatM (replicate nTimeSteps (elTickIO Nothing)) e

tickIO :: StateT (Time, [Order], Elevator) IO ()
tickIO = do
  (now, os, el0) <- get
  
  if not (null os) && orderTime (head os) == now
  then do
    let (os1, os2) = getNowOrders os
    el1 <- liftIO $ foldM (\o -> processOrderIO o) el0 os1
    put (now+1, os2, el1)
  else do
    el1 <- liftIO $ continueIO 1 el0
    put (now+1, os, el1)

processOrderIO :: Elevator -> Order -> IO Elevator
processOrderIO el (MkOrder _ iD start end) =
  elTickIO (Just (PickUpG (MkRider iD start end))) el
-}


-----------------------------------------------------------------------

{-
addGoal2 :: Goal -> [Goal] -> [Elevator] -> [Elevator]
        -> [Elevator]

addGoal2 g otherGoals okEls otherEls1 = els2
  where
    (goalEl0, otherEls2) = findClosestElToGoal g okEls

    compatGs = if isPickUpGoal g
               then g : compatiblePickUpGoals g otherGoals
               else [g]

    goalEl1 = foldr (\g' -> acceptNewGoal2 g') goalEl0 compatGs
    els1 = goalEl1 : (otherEls1 ++ otherEls2)
    els2 = sortBy (\ x y -> compare (eId x) (eId y)) els1
-}


{-
advanceEl :: Elevator -> Operation -> Elevator

advanceEl e MoveTowardNextGoal = cmd e
  where
    cmd = case elDir e of
            GoingUp   -> moveUp
            GoingDown -> moveDown
            Waiting   -> wait
      
advanceEl e Wait          = e
advanceEl e (PickUpOp  _) = pickUpRider2  e
advanceEl e DropOffOp     = dropOffRider2 e
-}



{-
elTick0 :: Elevator -> Elevator
elTick0 el@(MkElevator _ _ []  ) = el

elTick0 e@(MkElevator _ f (g:_)) =
  if f == goalFloor g
  then case g of
    PickUpG  r -> advanceEl e (PickUpOp r)
    DropOffG _ -> advanceEl e  DropOffOp
  else advanceEl e MoveTowardNextGoal    
-}
       


{- 


{-
advanceEl e (PickUpOp  r) = e{eGoals = pickUpRider  (eCurrFloor e) (eGoals e) r}
advanceEl e DropOffOp     = e{eGoals = dropOffRider (eGoals e)}
-}

pickUpRider :: Floor -> [Goal] -> Rider -> [Goal]
pickUpRider currF (g:gs) r =
  case g of 
    DropOffG _ -> error "pickUpRider: head is DropOffG"
    PickUpG  r -> acceptNewGoal currF (g:gs) (DropOffG r)


dropOffRider :: [Goal] -> [Goal]
dropOffRider [] = []
dropOffRider (g:gs) =
  case g of
    PickUpG  _ -> error "dropOffRider: head is PickUpG"
    DropOffG _ -> gs
{-
    DropOffG _ -> if null gs
                  then []
                  else gs
-}



{-  where gs  = head gss
        ggs = tail gss
-}    


{-
-- only for PickUpG.
-- Add to goals if goals is null
-- add to goals only if start startFloor is between the currF
-- and the floor of the last goal

addToGoals :: Floor -> [Goal] -> Goal -> Maybe [Goal]
addToGoals _ [] g = Just [g]

addToGoals currF gqs@(g:gs) goal@(PickUpG (MkRider rStart _)) = 
    if isBetweenInclusive rStart currF lastFloor && error "goingInSameDir"
    then Just $ acceptNewGoal currF gqs goal
    else Nothing
  where
    lastFloor = goalFloor $ last gs

addToGoals _ _ _ = error "addToGoals: must be PickUpG"
-}

--addGoalToEl :: Goal -> Elevator -> Elevator
--addGoalToEl g el = el{eGoals = g : eGoals el}

-- acceptNewGoal _ _ _ = error "acceptNewGoal: goal not accepted"

-- put in goal queue only if start is between
-- currF and floor of last goal, otherwise put one of the other
-- queues

                    
acceptNewGoal :: Floor -> [Goal] -> Goal -> [Goal]
acceptNewGoal currF gs g =
  case goalsDir currF gs of
    Waiting   -> [g]
    GoingUp   -> insert g gs
    GoingDown -> (reverse $ insert g $ reverse gs)


goalsDir :: Floor -> [Goal] -> Direction
goalsDir _     [] = Waiting
goalsDir currF gs = direction currF $ goalFloor $ last gs
    


-}



{-
processGoal2 :: Goal -> [Elevator] -> Maybe [Elevator]

processGoal2 g els | trace ("processGoal2:" 
                           ++ "\n g: " ++ show g
                           ++ "\n els: " ++ show els
                          ) False = undefined

processGoal2 g els0 = mEls1
  where
--    g    = mkGoal o
    (passingByEls_tr, otherEls1) = partition (elIsPassingBy g) els0
    (idleEls, otherEls2) = partition elIsIdle els0
    
    mEls1 = if not $ null passingByEls
           then Just $ addGoal g passingByEls otherEls1
           else
             if not $ null idleEls
             then Just $ addGoal g idleEls otherEls2
             else Nothing

    passingByEls = trace ("passingByEls: " ++ show passingByEls_tr ++ "\n")
                         passingByEls_tr
-}


--------------
{-
r1 = MkRider 1 1 3
r2 = MkRider 2 1 3
g1 = PickUpG r1
g2 = PickUpG r2
el1 = MkElevator 1 1 [g2]

b1 = elIsPassingBy g1 el1

gs1 = [g2]
cf1 = 1
dir1 = direction cf1 $ goalFloor $ head gs1

-}

{-
o1 = MkOrder 1 1 1 3
o2 = MkOrder 1 2 1 3
os = [o1, o2]

els = map (\i ->  MkElevator i 1 []) [1..2]
b0 = MkBuilding 10 els os [] []  0.1 1 1 
-}


{-
r3 = MkRider 3 1 10
r4 = MkRider 4 1 10
r5 = MkRider 5 1 10
g3 = PickUpG r3
g4 = PickUpG r4
g5 = PickUpG r5
e1 = MkElevator 1 10 []

els2 = addGoal2 g3 [g4, g5] [e1] []
-}
