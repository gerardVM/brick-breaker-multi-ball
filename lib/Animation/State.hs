module Animation.State where

import Control.Monad.Trans.State.Strict (get, put)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Class (lift)
import System.IO (hReady, Handle(..), stdin, stdout, hSetEcho, hSetBuffering, BufferMode(NoBuffering))
import Data.Char (toUpper, toLower)

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
        { position     :: (Int, Int)
        , direction    :: (Direction, Direction)
        , bXPosition   :: Int
        , bricks       :: [Object]
        , walls        :: Maybe Object
        , points       :: Int
        , userInputs   :: [UserInput]
        , status       :: GameStatus
        }

-- | Allocation of the list of reduced positions in the game
-- | A reduced position is a 'x' value divided by the brick length
-- | Positions in this function are a list of 'x positions'. This means that
-- | given width = 50 then positions 49, 50, 51, 52,... correspond to points (49,0), (50,0), (1,1), (2,1),...

bricksInPlace :: Int -> [Int] -> Int -> Int -> [Object]
bricksInPlace width positions life bricklength = map (\x -> Brick (findPosition (bricklength*x) width 0) life) positions
           where findPosition x width level = if x < width then (x,level) else findPosition (x - width) width (level + 1)

-- | Default state

defaultSt :: St
defaultSt = St (0, 0) (Neutral, Neutral) 0 [] Nothing 0 [] Stopped

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
                                                | x `isChar` 'r' = Restart   
                                                | x `isChar` ' ' = Restart   
                                                | x `isChar` 'j' = LeftWall  
                                                | x `isChar` 'l' = RightWall 
                                                | otherwise      = Undefined 
                            isChar c1 c2 = c1 == toLower c2 || c1 == toUpper c2 

next :: Animation Env St ()
next = do
    env    <- ask
    prevSt <- lift get
    input  <- lift $ lift $ getUserInput
    lift ( put ( nextInternal env input prevSt ) )

nextInternal :: Env -> Maybe UserInput -> St -> St
nextInternal (Env _ _ (width, height) velocity baselength bricklength _ _ _ wallHeight wallsGap) 
             userInput
             prevSt@(St (prevX, prevY) (prevXDir, prevYDir) prevBXPos prevBricks prevWalls prevPoints readInputs prevStatus)
             =
    
 -- | Management of next state according to GameStatus and UserInput. UserInput is included in State to avoid the repetition delay issue.
   
    case prevStatus of
        Paused        -> case userInput of 
                              Just Start   -> prevSt {status = Playing}
                              Just Stop    -> prevSt {status = Stopped}
                              _            -> prevSt
        Stopped       -> case userInput of 
                              Just Restart -> prevSt {status = Restarted}
                              _            -> prevSt
        LevelComplete -> case userInput of 
                              Just Restart -> prevSt {status = Restarted}
                              _            -> prevSt
        Playing       -> if prevBricks /= [] then
                            case userInput of 
                                Just Stop  -> prevSt {status = Stopped}
                                Just Pause -> prevSt {status = Paused }

                             -- | Update the state of the base and read it. Repeating Input or reaching limit interrupts the action
                                
                                Just MoveLeft ->
                                      if elem MoveRight readInputs || elem MoveLeft readInputs
                                        then St                                         
                                               { position   = (newX, newY)
                                               , direction  = (newXDir, newYDir)
                                               , bXPosition = prevBXPos
                                               , bricks     = newBricks
                                               , points     = newPoints
                                               , status     = newStatus
                                               , walls      = newWalls
                                               , userInputs = newInputs
                                               }                            
                                        else St 
                                               { position   = (newX, newY)
                                               , direction  = (newXDir, newYDir)
                                               , bXPosition = newBXPosition (-2)
                                               , bricks     = newBricks
                                               , points     = newPoints
                                               , status     = newStatus
                                               , walls      = newWalls
                                               , userInputs = MoveLeft : newInputs
                                               }
                                Just MoveRight ->
                                      if elem MoveLeft readInputs || elem MoveRight readInputs 
                                        then St                                         
                                               { position   = (newX, newY)
                                               , direction  = (newXDir, newYDir)
                                               , bXPosition = prevBXPos
                                               , bricks     = newBricks
                                               , points     = newPoints
                                               , status     = newStatus
                                               , walls      = newWalls
                                               , userInputs = newInputs
                                               }
                                        else St 
                                               { position   = (newX, newY)
                                               , direction  = (newXDir, newYDir)
                                               , bXPosition = newBXPosition (2)
                                               , bricks     = newBricks
                                               , points     = newPoints
                                               , status     = newStatus
                                               , walls      = newWalls
                                               , userInputs = MoveRight : newInputs
                                               }
                                _  -> 
                                      if elem MoveLeft readInputs && prevBXPos > 0 
                                        then St 
                                               { position   = (newX, newY)
                                               , direction  = (newXDir, newYDir)
                                               , bXPosition = newBXPosition (-2)
                                               , bricks     = newBricks
                                               , points     = newPoints
                                               , status     = newStatus
                                               , walls      = newWalls
                                               , userInputs = MoveLeft : newInputs
                                               }
                                 else if elem MoveRight readInputs && prevBXPos + baselength < width
                                        then St 
                                               { position   = (newX, newY)
                                               , direction  = (newXDir, newYDir)
                                               , bXPosition = newBXPosition ( 2)
                                               , bricks     = newBricks
                                               , points     = newPoints
                                               , status     = newStatus
                                               , walls      = newWalls
                                               , userInputs = MoveRight : newInputs
                                               }
                                        else St 
                                               { position   = (newX, newY)
                                               , direction  = (newXDir, newYDir)
                                               , bXPosition = prevBXPos
                                               , bricks     = newBricks
                                               , points     = newPoints
                                               , status     = newStatus
                                               , walls      = newWalls
                                               , userInputs = newInputs
                                               }
                         else prevSt {status = LevelComplete }

    where
 
 -- | New_Unbounded tells us which would be the position of the ball if there is no bound (Very handy)
   
    newXUnbounded          = prevX + directionToMultiplier prevXDir * velocity
    newYUnbounded          = prevY + directionToMultiplier prevYDir * velocity

 -- | Detection of collision with the base
    
    baseCollision          = prevBXPos <= newX && prevBXPos + baselength >= newX
                                               && newYUnbounded >= height - 2
 
 -- | Auxiliary functions to consider the length of a brick, not just their position
 -- | completePositions returns a list of occupied positions given a list of Bricks
    
    addPositions (u,v) brl = zip [u .. (u + brl - 1)] $ take brl $ repeat v
    completePositions      = foldl (\x y -> x ++ addPositions (brickPosition y) bricklength) []
    
 -- | Identification of the coordinate that will be impacted according to ball direction for three 
 -- | different cases: Collision with top or botton (brickCollisionY), collision with one side (brickCollisionX)   
 -- | or collision with a corner (cornerCollision)
    
    targetX                = ( newXUnbounded + directionToMultiplier prevXDir, newY)
    targetY                = ( newX, newYUnbounded + directionToMultiplier prevYDir)
    cornerTarget           = ( newXUnbounded + directionToMultiplier prevXDir
                             , newYUnbounded + directionToMultiplier prevYDir )
    brickCollisionY        = elem targetY $ completePositions prevBricks
    brickCollisionX        = elem targetX $ completePositions prevBricks
    cornerCollision        = not brickCollisionX && not brickCollisionY 
                           && elem cornerTarget (completePositions prevBricks)

    wallCollisionX         = case prevWalls of
                                 Just wall -> wallCollision wall targetX
                                 Nothing -> False
    wallCollisionY         = case prevWalls of
                                 Just wall -> wallCollision wall targetY
                                 Nothing -> False
    wallCornerCollision    = case prevWalls of
                                 Just wall -> wallCollision wall cornerTarget && not wallCollisionX && not wallCollisionY
                                 Nothing -> False
    
    wallCollision (Wall w) target = let condition x = x == fst target 
                                                   && snd target >= div (height - 2 - wallHeight) 2 
                                                   && snd target <= div (height - 2 + wallHeight) 2
                                     in case w of (Left  xPos) -> condition xPos 
                                                  (Right xPos) -> condition xPos

 -- | Identification of the block that will be hit.
    
    targetBlockY           = identify targetY      prevBricks
    targetBlockX           = identify targetX      prevBricks
    targetBlockC           = identify cornerTarget prevBricks

 -- | Filters the only brick that is hit by the ball given a target position and a list of Bricks.
    
    identify target        = head . filter (\u -> snd target == snd (brickPosition u) 
                                               && fst target -  fst (brickPosition u) < bricklength 
                                               && fst target -  fst (brickPosition u) >= 0          )
    
 -- | Update positions and directions for next state
    
    newX =
        case prevXDir of
            Neutral  ->     newXUnbounded
            Positive -> min newXUnbounded width
            Negative -> max newXUnbounded 0
    newY =
        case prevYDir of
            Neutral  ->     newYUnbounded
            Positive -> min newYUnbounded height
            Negative -> max newYUnbounded 0
    newXDir =
        case prevXDir of
            Neutral  -> Neutral
            Positive ->
                if newXUnbounded >= width  || brickCollisionX || cornerCollision || wallCollisionX || wallCornerCollision
                then Negative
                else Positive
            Negative ->
                if newXUnbounded <= 0      || brickCollisionX || cornerCollision || wallCollisionX || wallCornerCollision
                then Positive
                else Negative
    newYDir =
        case prevYDir of
            Neutral  -> Neutral
            Positive -> if brickCollisionY || cornerCollision || baseCollision   || wallCollisionY || wallCornerCollision
                            then Negative
                            else Positive
            Negative ->
                if newYUnbounded <= 0      || brickCollisionY || cornerCollision || wallCollisionY || wallCornerCollision
                then Positive
                else Negative
    
 -- | Position control of the base limited by the width
    
    newBXPosition i = let newBxPos = prevBXPos + i
                       in if newBxPos + baselength > width
                          then prevBXPos
                          else if newBxPos <= 0
                               then 0
                               else newBxPos
    
 -- | Update status in case the player is unable to bounce back the ball
    
    newStatus = if newY /= height then Playing else Stopped
 
 -- | Update the score in case of any brick collision 
    
    newPoints = (+) prevPoints $ fromEnum $ brickCollisionY || brickCollisionX || cornerCollision
    
 -- | Update the bricks state according to collisions. Brick disappears if life = 0
    
    newBricks   -- | Case 1: Collision in Y axis AND X axis (Two bricks at the same time)
              
              | brickCollisionX && brickCollisionY 
                                = changeBricks targetBlockY $ changeBricks targetBlockX prevBricks
             
                -- | Case 2: Collision in Y axis
              
              | brickCollisionY = changeBricks targetBlockY prevBricks
             
                -- | Case 3: Collision in X axis
             
              | brickCollisionX = changeBricks targetBlockX prevBricks
             
                -- | Case 4: Collision with a corner
             
              | cornerCollision = changeBricks targetBlockC prevBricks
             
                -- | Case 5: No collision
             
              | otherwise = prevBricks
 
 -- | Update the life of the bricks
    
    changeBricks x bricks = let brickTail  = filter ((/=) (brickPosition x) . brickPosition) bricks
                                brickHurt  = Brick (brickPosition x) (life x - 1)
                             in if life x > 0 then brickHurt : brickTail else brickTail

 -- | Update the state of the walls. Repeating Input interrupts the action

    newWalls = if elem RightWall readInputs then
                  case userInput of
                       Just RightWall -> Nothing
                       Just LeftWall  -> Just $ Wall $ Left  wallsGap
                       _              -> Just $ Wall $ Right (width - wallsGap)
          else if elem LeftWall  readInputs then
                  case userInput of
                       Just LeftWall  -> Nothing
                       Just RightWall -> Just $ Wall $ Right (width - wallsGap)
                       _              -> Just $ Wall $ Left  wallsGap
             else case userInput of
                       Just LeftWall  -> Just $ Wall $ Left  wallsGap
                       Just RightWall -> Just $ Wall $ Right (width - wallsGap)
                       _              -> Nothing

 -- | Save the state of the walls and delete states of the base

    newInputs = case newWalls of 
                     Just (Wall (Left _))  -> [LeftWall] 
                     Just (Wall (Right _)) -> [RightWall] 
                     _ -> [] 
                           