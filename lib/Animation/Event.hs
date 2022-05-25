module Animation.Event where
{-
import Data.IORef

handleInput :: Double -> IORef Game -> Int
handleInput dt game = do
    when `pressed` LEFT   $ game $~ movePaddle playerY dt
    when `pressed` RIGHT  $ game $~ movePaddle playerY (-dt)
    -}