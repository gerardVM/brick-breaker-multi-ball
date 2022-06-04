module Animation.State where

import Control.Monad.Trans.State.Strict (get, put)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Class (lift)
import System.IO (hReady, Handle(..), stdin, hSetEcho, hSetBuffering, BufferMode(NoBuffering))
import Data.Char (toUpper, toLower)
import Data.List (nubBy)

import Animation.Env (Env(..))
import Animation.Type ( Animation
                      , GameStatus(..)
                      , UserInput(..)
                      , Object(..)
                      )

data Direction
    = Positive
    | Negative
    | Neutral
    deriving Eq

directionFromInt :: Int -> Direction
directionFromInt 0 = Neutral
directionFromInt 1 = Positive
directionFromInt 2 = Negative
directionFromInt _ = error "Boooooo....."

directionToMultiplier :: Direction -> Int
directionToMultiplier Positive =  1
directionToMultiplier Negative = -1
directionToMultiplier Neutral  =  0

data St =
    St
        { position     :: [(Int, Int)]
        , direction    :: [(Direction, Direction)]
        , bXPosition   :: Int
        , bricks       :: [Object]
        , multiplier   :: [Object]
        , points       :: Int
        , userInputs   :: [UserInput]
        , status       :: GameStatus
        , clock        :: Int
        }

{-|
  Allocation of the list of reduced positions in the game
  A reduced position is a 'x' value divided by the brick length
  Positions in this function are a list of 'x positions'. This means that
  given width = 50 then positions 49, 50, 51, 52,... correspond to points (49,0), (50,0), (1,1), (2,1),...
-}
bricksInPlace :: Int -> [Int] -> Int -> Int -> [Object]
bricksInPlace width positions life bricklength = map (\x -> Brick (findPosition (bricklength*x) width 0) life) positions
           where findPosition x width level = if x < width then (x,level) else findPosition (x - width) width (level + 1)

-- | Default state

defaultSt :: St
defaultSt = St [] [] 0 [] [] 0 [] Stopped 0

-- | Management of the User Input

getUserInput :: IO (Maybe UserInput)
getUserInput = go Nothing
        where go a = do
                hSetBuffering stdin NoBuffering 
                hSetEcho stdin False
                waiting   <- hReady stdin
                if not waiting then return Nothing
                else do
                      hSetBuffering stdin NoBuffering
                      key <- getChar
                      more <- hReady stdin
                      (if more then go else return) $ Just (stringToUserInput key)
                      where stringToUserInput x | x `isChar` 'a' = MoveLeft  
                                                | x `isChar` 'd' = MoveRight 
                                                | x `isChar` 'p' = Pause     
                                                | x `isChar` 'q' = Stop      
                                                | x `isChar` 's' = Start     
                                                | x `isChar` 'r' = RestAuto   
                                                | x `isChar` ' ' = RestAuto
                                                | otherwise      = Undefined 
                            isChar c1 c2 = c1 == toLower c2 || c1 == toUpper c2 

next :: Animation Env St ()
next = do
    env    <- ask
    prevSt <- lift get
    input  <- lift $ lift $ getUserInput
    lift ( put ( nextInternal env input prevSt ) )

nextInternal :: Env -> Maybe UserInput -> St -> St
nextInternal (Env _ _ (width, height) velocity baselength bricklength _ _ maxBalls multFreq multSlow) 
             userInput
             prevSt@(St prevPositions prevDirections prevBXPos prevBricks prevMultipliers prevPoints readInputs prevStatus clock)
             =
    
    -- Management of next state according to GameStatus and UserInput. UserInput is included in State to avoid the repetition delay issue.
   
    case prevStatus of
        Paused        -> case userInput of 
                              Just Pause    -> prevSt {status = Playing}
                              Just Stop     -> prevSt {status = Stopped}
                              Just RestAuto -> prevSt {status = Auto   }
                              _             -> prevSt
        Stopped       -> case userInput of 
                              Just RestAuto -> prevSt {status = Restarting}
                              _             -> prevSt
        Starting      -> case userInput of 
                              Just RestAuto -> prevSt {status = Restarting }
                              Just Start    -> prevSt {status = Playing    }     
                              _             -> prevSt { position   = [(newBXPos + div baselength 2, snd $ head prevPositions)]
                                                      , bXPosition = newBXPos 
                                                      , userInputs = newInputs }
        LevelComplete -> case userInput of 
                              Just RestAuto -> prevSt {status = Restarting}
                              _             -> prevSt
        Playing       -> if prevBricks /= [] then
                            case userInput of 
                                Just Stop      -> prevSt {status = Stopped}
                                Just Pause     -> prevSt {status = Paused }
                                Just RestAuto  -> prevSt {status = Auto   }
                                _  -> St 
                                           { position   = newPositions
                                           , direction  = newDirections
                                           , bXPosition = newBXPos
                                           , bricks     = newBricks
                                           , multiplier = newMultipliers
                                           , points     = newPoints
                                           , userInputs = newInputs
                                           , status     = newStatus
                                           , clock      = newClock
                                           }
                         else prevSt {status = LevelComplete }
        Auto          -> if prevBricks /= [] then
                            case userInput of 
                                Just Stop    -> prevSt {status = Stopped    }
                                Just Pause   -> prevSt {status = Paused     }
                                _  -> St 
                                           { position   = newPositions
                                           , direction  = newDirections
                                           , bXPosition = newBXPos
                                           , bricks     = newBricks
                                           , multiplier = newMultipliers
                                           , points     = newPoints
                                           , userInputs = newInputs
                                           , status     = newStatus
                                           , clock      = newClock
                                           }
                         else prevSt {status = LevelComplete }

    where

 -- Adding positions and directions for freshly created balls

    newBallsPos = foldr (\pos -> (++) $ take 3 $ repeat pos) [] prevPositions
    newBallsDir = foldr (\dir -> (++) $ filter (/= dir) [ (xDir,yDir) | xDir <- [Positive, Negative], yDir <- [Positive, Negative] ]) [] prevDirections

 -- Removing positions and directions for lost balls

    removeBalls = filter (\pair -> snd (fst pair) < height )  -- CHANGE FOR THE EASY FUNCTION

 -- New_Unbounded tells us which would be the position of the ball if there is no bounding. Also we need to know just edge bounded positions.
   
    newXUnbounded          = map (\xPair -> fst xPair + directionToMultiplier (snd xPair) * velocity) xPosDirPair 
    newYUnbounded          = map (\yPair -> fst yPair + directionToMultiplier (snd yPair) * velocity) yPosDirPair

    newUnboundedPositions  = zip newXUnbounded newYUnbounded

 -- Following info is necessary to check if new position is empty. Problematic case: ███  « 
 --                                                                                     ● «
 --                                                                                     ███ 

    newXBounced pair       = ( posX pair + directionToMultiplier (dirX pair) * velocity * (-1), posY pair + directionToMultiplier (dirY pair) * velocity )
    newYBounced pair       = ( posX pair + directionToMultiplier (dirX pair) * velocity, posY pair + directionToMultiplier (dirY pair) * velocity * (-1) )

 -- Zipping positions with directions for easier managing of values

    xPosDirPair            = zip (map fst prevPositions) (map fst prevDirections)
    yPosDirPair            = zip (map snd prevPositions) (map snd prevDirections)

 -- Position control of the base limited by the width - Repeating Input interrupts the action
    
    newBXPos   = case prevStatus of 
                      Auto -> if 3 > abs ( prevBXPos + div baselength 2 - fst ( head prevPositions ) ) then
                              restricted ( (fst $ head prevPositions) - div baselength 2)
                              else moveBase $ 3 * ( signum $ fst ( head prevPositions ) - prevBXPos )
                      _    -> baseDecisionTree userInput readInputs (moveBase (-2)) (moveBase ( 2)) prevBXPos 

    moveBase i = let newBxPos = prevBXPos + i in restricted newBxPos

    restricted position = if position + baselength >= width
                          then width - baselength
                          else if position <= 0
                               then 0
                               else position

 -- Detection of collision with the base

    baseCollision pair  = posY pair >= height - 2 && newBXPos <= posX pair && posX pair <= newBXPos + baselength      

    baseCornerCollision pair = posY pair >= height - 2 
                            && not ( baseCollision pair )
                            && ( newBXPos              <= posX pair + directionToMultiplier (dirX pair) 
                            &&   newBXPos + baselength >= posX pair + directionToMultiplier (dirX pair) )

 -- Auxiliary functions to consider the length of a brick, not just their position
 -- completePositions returns a list of occupied positions given a list of Bricks
    
    addPositions (u,v) brl = zip [u .. (u + brl - 1)] $ take brl $ repeat v
    completePositions      = foldl (\x y -> x ++ addPositions (brickPosition y) bricklength) []
    
 -- Identification of the coordinate that will be impacted according to ball direction for three 
 -- different cases: Collision with top or botton (brickCollisionY), collision with one side (brickCollisionX)   
 -- or collision with a corner (cornerCollision)

    singleTargetX pair     = ( posX pair + directionToMultiplier (dirX pair), posY pair)
    singleTargetY pair     = ( posX pair, posY pair + directionToMultiplier (dirY pair))
    singleTatgetC pair     = ( posX pair + directionToMultiplier (dirX pair) 
                             , posY pair + directionToMultiplier (dirY pair) )

    brickCollisionX pair   = elem (singleTargetX pair) (completePositions prevBricks)
    brickCollisionY pair   = elem (singleTargetY pair) (completePositions prevBricks) 
    cornerCollision pair   = elem (singleTatgetC pair) (completePositions prevBricks)

    bounceX pair          = brickCollisionX pair && newXPosAvailable pair
    bounceY pair          = brickCollisionY pair && newYPosAvailable pair
    bounceC pair          = cornerCollision pair && not (bounceX pair) && not (bounceY pair)
                         || ( brickCollisionX pair && not (newXPosAvailable pair) )
                         || ( brickCollisionY pair && not (newYPosAvailable pair) )

    newXPosAvailable pair = not $ elem (newXBounced pair) (completePositions prevBricks)
    newYPosAvailable pair = not $ elem (newYBounced pair) (completePositions prevBricks)

 -- Identification of the block that will be hit.

    targetBrick pair      = if bounceX pair && bounceY pair then map identify $ [singleTargetX pair] ++ [singleTargetY pair]
                       else if bounceX pair then map identify [singleTargetX pair]
                       else if bounceY pair then map identify [singleTargetY pair]
                       else if bounceC pair then if cornerCollision pair                                then map identify [singleTatgetC pair]
                                            else if brickCollisionX pair && not (newXPosAvailable pair) then map identify $ [newXBounced pair] ++ [singleTargetX pair]
                                                                                                        else map identify $ [newYBounced pair] ++ [singleTargetY pair]
                                                                                                        else []

    targetBricks           = foldl (++) [] $ map targetBrick $ newUnboundedPositionsDirectionsPairs 

 -- Filters the only brick that is hit by the ball given a target position and a list of Bricks.
    
    identify target         = let result = filter (\u -> snd target == snd (brickPosition u) 
                                                      && fst target -  fst (brickPosition u) < bricklength 
                                                      && fst target -  fst (brickPosition u) >= 0          ) prevBricks
                               in if result == [] then Nothing else Just $ head result

 -- Update positions and directions for next state
    
    newX    = map ( \pair -> case dirX pair of
                                Neutral  ->     (posX pair)
                                Positive -> min (posX pair) width
                                Negative -> max (posX pair) 0
                  ) newUnboundedPositionsDirectionsPairs

    newY    = map ( \pair -> case dirY pair of
                                Neutral  ->     (posY pair)
                                Positive -> min (posY pair) height
                                Negative -> max (posY pair) 0
                  ) newUnboundedPositionsDirectionsPairs

    newXDir = map ( \pair -> case dirX pair of
                                Neutral  -> Neutral
                                Positive ->
                                    if posX pair >= width  || baseCornerCollision pair || bounceX pair || bounceC pair
                                    then Negative
                                    else Positive
                                Negative ->
                                    if posX pair <= 0      || baseCornerCollision pair || bounceX pair || bounceC pair
                                    then Positive
                                    else Negative
                  ) newUnboundedPositionsDirectionsPairs

    newYDir = map ( \pair -> case dirY pair  of
                                Neutral  -> Neutral
                                Positive -> 
                                    if posY pair >= height || baseCollision pair || baseCornerCollision pair || bounceY pair || bounceC pair
                                    then Negative
                                    else Positive
                                Negative -> 
                                    if posY pair <= 0      || bounceY pair || bounceC pair
                                    then Positive
                                    else Negative
                  ) newUnboundedPositionsDirectionsPairs

-- Definition of new Positions and Directions of balls making sure that they are not overlapping 

    (newPositions, newDirections) = let newPositionsDirectionsPairs = take maxBalls $ nubBy (==) $ zip
                                                                      ( zip newX    newY    ++ (if gotMultiplier then newBallsPos else []) ) 
                                                                      ( zip newXDir newYDir ++ (if gotMultiplier then newBallsDir else []) )
                                     in unzip newPositionsDirectionsPairs

-- Creating pairs of position - direction for managment of new states

    newUnboundedPositionsDirectionsPairs = removeBalls $ zip newUnboundedPositions prevDirections

    (posX, posY, dirX, dirY) = (fst . fst , snd . fst, fst . snd, snd . snd)
    
 -- Update status in case the player is unable to bounce back the ball
    
    newStatus = if length newPositions == 0 then Stopped else prevStatus
 
 -- Update the score in case of any brick collision 
    
    newPoints = prevPoints + (length prevBricks - length newBricks)
    
 -- Update the bricks state according to collisions. Brick disappears if life == 0
    
    newBricks = foldl (flip changeBricks) prevBricks targetBricks

 -- Update the life of the bricks
    
    changeBricks target bricks = case target of 
                                    Just x -> let brickTail  = filter ((/=) (brickPosition x) . brickPosition) bricks
                                                  brickHurt  = Brick (brickPosition x) (life x - 1)
                                               in if life x > 0 then brickHurt : brickTail else brickTail
                                    _      -> bricks

 -- Read & record the UserInputs - Repeating Input interrupts the action

    newInputs = baseDecisionTree userInput readInputs [MoveLeft] [MoveRight] []
                           
 -- Decision helper function

    baseDecisionTree uInput rInput efect1 efect2 noEfect = case uInput of 
                                           Just MoveLeft  -> if not $ elem MoveRight rInput || elem MoveLeft  rInput then efect1 else noEfect
                                           Just MoveRight -> if not $ elem MoveLeft  rInput || elem MoveRight rInput then efect2 else noEfect
                                           _              -> if       elem MoveLeft  rInput && prevBXPos > 0                  then efect1
                                                              else if elem MoveRight rInput && prevBXPos + baselength < width then efect2
                                                                                                                              else noEfect

-- Management of the Multipliers: Creation, desrtuction and velocity

    newMultipliers  = let removeMultipliers                     = filter ( \multi@(Multiplier (x,y)) -> y < height + 1 && not (baseMultiplier multi) )
                          createABrickBonus (targetBrick : tBs) = case targetBrick of
                                                                      Just (Brick position _) -> [Multiplier position]
                                                                      _                       -> createABrickBonus tBs
                          createABrickBonus  _                  = []
                       in removeMultipliers $ map advanceInY prevMultipliers 
                       ++ if newPoints `mod` multFreq == 0 && (length prevBricks /= length newBricks) then createABrickBonus targetBricks else []

    advanceInY (Multiplier (x,y)) = let fallControl = if mod clock multSlow < 1 then 1 else 0 in Multiplier (x , y + fallControl )    

-- Catching Multipliers

    baseMultiplier (Multiplier (multX, multY)) = multY == height - 1 && newBXPos <= multX && multX <= newBXPos + baselength

    gotMultiplier = length prevMultipliers > length ( filter (not . baseMultiplier) (map advanceInY prevMultipliers) )

-- Keeping track of frames
 
    newClock = clock + 1