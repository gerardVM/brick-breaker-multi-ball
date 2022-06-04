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
        , fps         = 20
        , size        = (75, 22)
        , velocity    = 1
        , baselength  = 15 * fst (size defaultEnv) `div` 100 -- ^ (%)
        , bricklength = 5  * fst (size defaultEnv) `div` 100 -- ^ (%)     
        , numOfBricks = 0
        , posOfBricks = []
        , lifes       = 2
        , wallsHeight = 60 * (snd (size defaultEnv) - 2) `div` 100 -- ^ (%)
        , wallsGap    = 20 *  fst (size defaultEnv)      `div` 100 -- ^ (%)
        }
