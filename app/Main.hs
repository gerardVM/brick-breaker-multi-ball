module Main where

import Control.Concurrent (threadDelay)
import System.Random (randomRIO)
import Data.List (nubBy)

import Control.Monad.Trans.State.Strict (put, get)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Levels.Levels (level_1)
import Animation.Type (GameStatus(..))
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


putInitialState :: Animation Env St ()
putInitialState = do
    env@(Env _ _ (width, height) _ baselength bricklength _ lifes _ _ _) <- ask
    dirX <- lift $ lift $ fmap directionFromInt $ randomRIO (1, 2)
    
    -- Creation of a random number of blocks limited by a desired maximum number.

    let maxBricks = div (width * (height - 4)) . (*) bricklength in do

        randNumBricks  <- lift $ lift $ randomRIO (0, maxBricks 4)
    
    -- Creation of a list of DIFFERENT positions. The range of available positions has to be divided by the bricklength so
    -- we can introduce the bricklength space afterwards in order to get bricks not to overlap
        
        distBricks <-  fmap (nubBy (==)) $ sequence $ replicate (randNumBricks) $ randomRIO (1, maxBricks 1)
             
    -- Giving parameters to our initial state
        
        lift $ put $ St [(div width 2, height - 2)]
                        [(dirX       , Negative  )]
                        (div (width - baselength) 2)
                        (level_1 env)
                        -- (bricksInPlace width distBricks lifes bricklength)
                        [] 
                        0 
                        [] 
                        Starting  
                        0  

 -- Management of the animation. Interrupted if game Restarted

animate :: Animation Env St ()
animate = do
    render
    st <- lift get
    env <- ask
    case (status st) of
        Restarting -> putInitialState
        _          -> next
    lift $ lift $ threadDelay $ 1000000 `div` (fps env)
    animate

mainAnimation :: Animation Env St ()
mainAnimation = do
    putInitialState
    animate

main :: IO ()
main = do
    runAnimation defaultEnv defaultSt mainAnimation
