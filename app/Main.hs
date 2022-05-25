module Main where

import Animation
    ( Animation
    , Direction(..)
    , Env(..)
    , St(..)
    , defaultEnv
    , defaultSt
    , next
    , render
    , runAnimation
    , directionFromInt
    , bricksInPlace
    )

import Control.Concurrent (threadDelay)
import System.Random (randomRIO)
import Data.List (nubBy)

import Control.Monad.Trans.State.Strict (put, get)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Animation.Type (GameStatus(..))

putInitialState :: Animation Env St ()
putInitialState = do
    (Env _ _ (width, height) _ baselength bricklength _ _ lifes _ _) <- ask
    posX <- lift $ lift $ randomRIO (div width  3, (*) 2 $ div width  3)
    posY <- lift $ lift $ randomRIO (div height 3, (*) 2 $ div height 3)
    dirX <- lift $ lift $ fmap directionFromInt $ randomRIO (1, 2)
    dirY <- lift $ lift $ fmap directionFromInt $ randomRIO (1, 2)
    let maxBlocks = div (width * (height - 4)) . (*) bricklength in do
 
    -- | Creation of a random number of blocks limited by a desired maximum number.

        randNumBlocks  <- lift $ lift $ randomRIO (0, maxBlocks 4)
    
    -- | Creation of a list of DIFFERENT positions. The range of available positions has to be divided by the bricklength so
    -- | we can introduce the bricklength space afterwards in order to get bricks not to overlap
        
        distBlocks <-  fmap (nubBy (==)) $ sequence $ replicate (randNumBlocks) $ randomRIO (1, maxBlocks 1)
             
    -- | Giving parameters to our initial state
        
        lift $ put $ St (posX, posY) 
                        (dirX, dirY) 
                        (div (width - baselength) 2) 
                        (bricksInPlace width distBlocks lifes bricklength) 
                        Nothing 
                        0 
                        [] 
                        Paused    

 -- | Management of the animation. Interrupted if game Restarted

animate :: Animation Env St ()
animate = do
    render
    st <- lift get
    env <- ask
    case (status st) of
        Restarted -> putInitialState
        _         -> next
    lift $ lift $ threadDelay $ 1000000 `div` (fps env)
    animate

mainAnimation :: Animation Env St ()
mainAnimation = do
    putInitialState
    animate

main :: IO ()
main = do
    runAnimation defaultEnv defaultSt mainAnimation
