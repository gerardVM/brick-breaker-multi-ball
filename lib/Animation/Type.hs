module Animation.Type where

import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.State.Strict (StateT(..), evalStateT)

type Animation env st a = ReaderT env (StateT st IO) a

data Object = Ball (Int, Int)
            | Base Int Int
            | Multiplier { multiplierPosition :: (Int, Int) }
            | Brick 
                    { brickPosition :: (Int, Int) 
                    , life :: Int 
                    }
        deriving ( Eq
                 , Show     -- ^ Show instance necessary for debugging
                )  
    
data GameStatus = Paused
                | Playing
                | Auto
                | Stopped
                | Starting
                | LevelComplete
                | Restarting
                deriving Show

data UserInput  = MoveLeft
                | MoveRight
                | Pause
                | Stop
                | Start
                | RestAuto
                | Undefined
                deriving Eq

runAnimation :: env -> st -> Animation env st a -> IO a
runAnimation env st action = evalStateT (runReaderT action env) st
