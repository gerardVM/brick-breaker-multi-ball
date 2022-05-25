module Animation.Type where

import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.State.Strict (StateT(..), evalStateT)

type Animation env st a = ReaderT env (StateT st IO) a

data Object = Ball Int
            | Base Int Int
            | Wall (Either Int Int)
            | Brick 
                    { brickPosition :: (Int, Int) 
                    , life :: Int 
                    }
        deriving Eq
    
data GameStatus = Paused
                | Playing
                | Stopped
                | LevelComplete
                | Restarted
                deriving (Show)

data UserInput  = MoveLeft
                | MoveRight
                | Pause
                | Stop
                | Start
                | Restart
                | LeftWall
                | RightWall
                | Undefined
                deriving (Eq) 

runAnimation :: env -> st -> Animation env st a -> IO a
runAnimation env st action = evalStateT (runReaderT action env) st
