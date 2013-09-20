{-# LANGUAGE ScopedTypeVariables #-}

module Elevators.Graphics where

-- * base
import Data.List (intercalate)
--import Debug.Trace (trace)

-- * gloss
import Graphics.Gloss

import Elevators.Building

--------------------------------------------------------------------------------
-- constants

windowWidthInt :: Int
windowWidthInt = 1000

windowHtInt :: Int
windowHtInt = 800

marginPct   :: Float
marginPct   = 10

ssMarginPct :: Float -- margin for all shafts
ssMarginPct = 30

elMarginPct :: Float -- margin for each elevator in a shaft
elMarginPct = 10 

elWidthMax  :: Float
elWidthMax  = 50

--------------------------------------------------------------------------------

centeredRectPict :: Float -> Float -> Picture
centeredRectPict w h = line [(x0, y0), (x0, y1), (x1, y1), (x1, y0), (x0, y0)]
  where
    x0 = - w / 2
    x1 =   w / 2
    y0 = - h / 2
    y1 =   h / 2

floorY :: Int -> Float -> Float -> Float
floorY iFloor floorHt bBottomY = fromIntegral (iFloor - 1) * floorHt + bBottomY

floorPict :: Float -> Float -> Float -> Float -> Float -> Float
          -> [Int] -> [Int] -> Int -> Picture
          
floorPict floorHt bLeftX bRightX sLeftX sRightX
          bBottomY waitingRs arrivedRs iFloor =
  Pictures [line path, fNumTxt , waitingTxt, arrivedTxt ]
  where
    path = [(bLeftX, fy), (bRightX, fy)]
    fy = floorY iFloor floorHt bBottomY
    
    fNumTxt =   Translate (bLeftX) (fy + 5 ) $ Scale 0.2 0.2
              $ Text (show iFloor)
    
    waitingTxt = Translate sLeftX  (fy + 5) $ Scale 0.1 0.1 $ Text (showEls waitingRs)
    arrivedTxt = Translate sRightX (fy + 5) $ Scale 0.1 0.1 $ Text (showEls arrivedRs)
    

elAtFloorInShaftPict :: Elevator -> Float -> Float -> Picture
-- elAtFloorInShaftPict _ elHt | trace ("elHt: " ++ show elHt) False = undefined

elAtFloorInShaftPict el elWidth elHt =
  Pictures [ centeredRectPict elWidth elHt
           , Scale 0.1 0.1 $ Text $ showEls $ getRiderIdsInElevator el
           ]


elInShaftPict :: Elevator -> Float -> Float -> Float -> Float -> Picture
elInShaftPict el floorHt bBottomY elWidth elHt =
  Translate 0 y $ elAtFloorInShaftPict el elWidth elHt
  where
    y = floorY iFloor floorHt bBottomY + floorHt / 2
    iFloor = eCurrFloor el


shaftPict :: Float -> Float -- -> Float
          -> Float -> Float -> Float -> Elevator -> Int
          -> Picture
                             
shaftPict floorHt sWidth {- sLeftX -} bBottomY elWidth elHt el iEl =
  Translate x 0 $ elInShaftPict el floorHt bBottomY elWidth elHt
  where
    x = -sWidth + fromIntegral iEl * sWidth
      
  
getInner :: Float -- ^ outer width
         -> Float -- ^ margin as a percent of outter width
         -> Float -- ^ inner width
getInner outer pct = outer * (1 - 2 * pct / 100)

getOuter :: Float -- ^ inner width
         -> Float -- ^ margin as a percent of outer
         -> Float -- ^ outer width
getOuter inner pct = inner / (1 - 2 * pct / 100)


drawBuilding ::  Building -> Picture
drawBuilding b@(MkBuilding nFloors els _ _ _ _ _ _) =
  bPict
  where
    windowHt    = fromIntegral windowHtInt
    windowWidth = fromIntegral windowWidthInt
      
    floorHt     = bHt / fromIntegral nFloors
      
    bWidth      = getInner windowWidth marginPct
    bHt         = getInner windowHt marginPct
    bLeftX      = - bWidth / 2
    bRightX     = bLeftX + bWidth
    bBottomY    = - bHt / 2
    bTopY       = bBottomY + bHt
      
    -- sXxx = shaftsXxx
    ssWidth  = getInner bWidth ssMarginPct
    sWidth   = ssWidth / fromIntegral (length els)
    elWidth  = getInner sWidth elMarginPct
    elHt     = getInner floorHt elMarginPct
    sLeftX   = - ssWidth / 2
    sRightX  =   ssWidth / 2
            
      
    bOutlinePict = centeredRectPict bWidth bHt 
    waiting      = mkRidersWaitingOnFloorLists b :: [[Rider]]   
    arrived      = mkRidersArrivedOnFloorLists b :: [[Rider]]
    
    floorsPict =
      Pictures $ map (\iF -> floorPict floorHt
                                       bLeftX
                                       bRightX
                                       sLeftX
                                       sRightX
                                       bBottomY
                                      (map rId (getRidersOnFloor iF waiting))
                                      (map rId (getRidersOnFloor iF arrived))
                                      iF
                     )
                     [1 .. nFloors]
                       
    mkShaftPict = shaftPict floorHt sWidth bBottomY elWidth elHt
            
    elsPict = Translate (- (ssWidth - sWidth) / 2) 0
              $ Pictures $ zipWith (\iEl el -> mkShaftPict el iEl)
                                   [1..] 
                                   els
                                   
    elapsedTimePict =
      Translate 0 bTopY $ Scale 0.2 0.2 $ Text $ show $ bElapsedTotalTime b
      
    bPict = Pictures [ elapsedTimePict, bOutlinePict
                     , floorsPict, elsPict
                     ]

--    waiting = trace ("waiting: " ++ show waiting_tr ++ "\n") waiting_tr

showEls :: Show a => [a] -> String
showEls xs = intercalate ", " $ map show xs
