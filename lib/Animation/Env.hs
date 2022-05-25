module Animation.Env where

import Animation.Type (UserInput(..))

data Env =
    Env
        { title       :: String
        , fps         :: Int
        , size        :: (Int, Int)
        , velocity    :: Int
        , baselength  :: Int
        , bricklength :: Int
        , numOfBricks :: Int
        , posOfBricks :: [Int]
        , lifes       :: Int
        , wallsHeight :: Int
        , wallsGap    :: Int
        }

defaultEnv :: Env
defaultEnv =
    Env { title       = "BRICK BREAKER VIDEOGAME"
        , fps         = 15
        , size        = (50, 20)
        , velocity    = 1
        , baselength  = 10
        , bricklength = 3        
        , numOfBricks = 0
        , posOfBricks = []
        , lifes       = 1
        , wallsHeight = 12
        , wallsGap    = 10
        }
