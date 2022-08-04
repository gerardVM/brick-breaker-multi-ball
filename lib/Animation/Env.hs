module Animation.Env where

import Animation.Type (UserInput(..))

data Env =
    Env
        { title       :: String     -- ^ Title of the Game
        , fps         :: Int        -- ^ Frames per second
        , size        :: (Int, Int) -- ^ Size of the game
        , velocity    :: Int        -- ^ Speed of the ball
        , baselength  :: Int        -- ^ Length of the base (%)
        , bricklength :: Int        -- ^ Length of the bricks
        , numOfBricks :: Int        -- ^ Number of bricks
        , lifes       :: Int        -- ^ Life of the bricks
        , maxBalls    :: Int        -- ^ Maximum number of balls
        , multFreq    :: Int        -- ^ Frequency of multipliers
        , multSlow    :: Int        -- ^ How slow the multiplier advances
        }

defaultEnv :: Env
defaultEnv =
    Env { title       = "BRICK BREAKER VIDEOGAME (MULTI-BALL)"
        , fps         = 20
        , size        = (80, 18)
        , velocity    = 1
        , baselength  = 10 * fst (size defaultEnv) `div` 100     
        , bricklength = 2
        , numOfBricks = 0
        , lifes       = 2
        , maxBalls    = 150
        , multFreq    = 3
        , multSlow    = 5
        }
