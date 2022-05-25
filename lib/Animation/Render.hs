module Animation.Render where

import Control.Monad.Trans.State.Strict (get)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Class (lift)

import Animation.Env (Env(..))
import Animation.State (St(..))
import Animation.Type (Animation,Object(..), GameStatus(..))

render :: Animation Env St ()
render = do
    val <- renderVal
    lift (lift (putStrLn val))

renderVal :: Animation Env St String
renderVal = do
    env <- ask
    st <- lift get
    return (renderInternal env st)

renderInternal :: Env -> St -> String
renderInternal env st = makeBox (size        env) 
                                (baselength  env) 
                                (bXPosition  st ) 
                                (position    st ) 
                                (bricklength env) 
                                (bricks      st )                                
                                (wallsHeight env)
                                (wallsGap    env) 
                                (walls       st ) 
                                (status      st )
                                (points      st )
                                (title       env)

-- | Definition of how each line is going to be rendered according to what is there. Oprions are:
-- | 1) Nothing / 2) Just bricks / 3) Just the ball / 4) Just the base / 5) The ball and bricks /
-- | 6) The ball and the base / (The base and bricks is not an option)
-- | New cases: 7) Just Wall / 8) Ball and Wall / 9) Wall and Bricks  / 10) Ball and Wall And Bricks 
-- | 11) All of them combined with the limits of the Smart Walls which always appear

makeLine :: Char 
         -> Char 
         -> Char 
         -> Int 
         -> Maybe Int
         -> Maybe Object 
         -> Maybe Object
         -> Maybe Object
         -> [Object] 
         -> Int
         -> String

makeLine eCh iCh wCh i mWG mBall mBase mWall bricks bricklength =
    let positions = [0 .. i]
        renderPixel x =
            case mBall of
                Nothing -> case mBase of 
                               Nothing           -> case mWG of 
                                                        Just wG -> if x == wG || x == i - wG then printBlock x '*' else printBlock x iCh
                                                        Nothing -> case mWall of
                                                                       Nothing                  -> printBlock x iCh
                                                                       Just ( Wall (Left  wx) ) -> if x == wx then printBlock x wCh else printBlock x iCh
                                                                       Just ( Wall (Right wx) ) -> if x == wx then printBlock x wCh else printBlock x iCh
                               Just (Base bl bx) -> if x `elem` [bx..(bx+bl)] then ':' else iCh
                                 
                Just (Ball b) -> case mBase of 
                               Nothing           -> case mWG of 
                                                        Just wG -> if x == b  then 'O' else if x == wG || x == i - wG then printBlock x '*' else printBlock x iCh
                                                        Nothing -> case mWall of
                                                                       Nothing                  -> if x == b  then 'O' else printBlock x iCh
                                                                       Just ( Wall (Left  wx) ) -> if x == b  then 'O' 
                                                                                              else if x == wx then printBlock x wCh 
                                                                                                              else printBlock x iCh
                                                                       Just ( Wall (Right wx) ) -> if x == b  then 'O' 
                                                                                              else if x == wx then printBlock x wCh 
                                                                                                              else printBlock x iCh
                               Just (Base bl bx) -> if x == b then 'O' 
                                                    else if x `elem` [bx..(bx+bl)] then ':' 
                                                    else iCh
                                
     in [eCh] ++ map renderPixel positions ++ [eCh]
     
     -- | Finding if a pixel should belong to a brick considering its position and length
     -- | If True, we paint it according to the life of the brick
     
     where brickXPositions = map (fst . brickPosition) bricks
           printBlock x ch = if x `elem` foldl (\u v  -> u ++ [v..(v+bricklength-1)]) [] brickXPositions
                             then if (life $ pixelOwner x) > 0 then '=' else '-'
                             else ch
           pixelOwner x    = head $ filter (\u -> x - fst (brickPosition u) < bricklength
                                               && x - fst (brickPosition u) >= 0 ) bricks

makeBox :: (Int, Int) 
        -> Int 
        -> Int 
        -> (Int, Int) 
        -> Int 
        -> [Object] 
        -> Int
        -> Int
        -> Maybe Object
        -> GameStatus
        -> Int
        -> String
        -> String
makeBox (numCols, numRows) baseL baseX (ballX, ballY) bricklength bricks wallHeight wG mWall status points title =
    unlines 
        ([take (div (numCols - length title + 4) 2) (repeat ' ') ++ title] ++ [" "] --  
        ++ case status of 
              LevelComplete -> [celebratrionCartoon]
              _             -> [   makeLine '-' '-' '·' numCols Nothing Nothing Nothing Nothing [] bricklength ]
                              ++   mappedPositions          
                              ++ [ makeLine '-' '-' '·' numCols Nothing Nothing Nothing Nothing [] bricklength ]
                              ++ ["Status: " ++ show status
                                 ++ if ballY /= numRows then   " | Score: " ++ show points 
                                    else  " | ***** GAME OVER ***** | Your Score is " ++ show points 
                                 ]
                         
                           -- | Render menu according to status
                         
                              ++ [ case status of
                                     Stopped       -> "Press (R) to Restart" ++ "\n" ++ "\n" -- ^ Necessary for inline space coherence
                                     Paused        -> "Press (S) to Play"    ++ "\nPlayer 1: (A) Move Left       / (D) Move Right" 
                                                                             ++ "\nPlayer 2: (J) Left Smart Wall / (L) Right Smart Wall"
                                     Playing       -> "(P) Pause / (Q) Stop" ++ "\n(A) P1: Move Left  (D) P1: Move Right"
                                                                             ++ "\n(J) P2: Left Wall  (L) P2: Right Wall"
                                     _             -> "\n" ++ "\n"                           -- ^ Necessary for inline space coherence
                                 ]

                           -- | Uncomment these lines for debugging purposes 

--                            ++ [  "BaseX: " ++ show (baseX + div baseL 2) 
--                               ++ " | Ball: (" ++ show ballX ++ "," ++ show ballY ++ ")"
--                               ++ " | BallOverBase: "   ++ show (ballX >= baseX && (ballX <= (baseX + baseL)))
--                               ]
        )

    where   positions       = [0 .. numRows]
            mappedPositions = map lineMaker positions

         -- | Painting lines depending on the Y of the different elements
            
            lineMaker y =
              let brickYPositions = filter ((==) y . snd . brickPosition) bricks
               in if y == ballY
                    then if y == numRows - 1
                         then           makeLine '|' ' ' ' ' numCols Nothing   (Just (Ball ballX)) (Just (Base baseL baseX)) Nothing brickYPositions bricklength
                         else if yWallEnds y
                              then      makeLine '|' ' ' '*' numCols (Just wG) (Just (Ball ballX)) Nothing                   mWall   brickYPositions bricklength
                         else if yWallRange y
                              then      makeLine '|' ' ' '║' numCols Nothing   (Just (Ball ballX)) Nothing                   mWall   brickYPositions bricklength
                              else      makeLine '|' ' ' ' ' numCols Nothing   (Just (Ball ballX)) Nothing                   Nothing brickYPositions bricklength
                    else if y == numRows - 1
                         then           makeLine '|' ' ' ' ' numCols Nothing   Nothing             (Just (Base baseL baseX)) Nothing brickYPositions bricklength
                         else if yWallEnds y 
                              then      makeLine '|' ' ' '*' numCols (Just wG) Nothing             Nothing                   mWall   brickYPositions bricklength
                         else if yWallUpperPart y && yWallRange y 
                              then      makeLine '|' ' ' '║' numCols Nothing   Nothing             Nothing                   mWall   brickYPositions bricklength
                         else if yWallLowerPart y && yWallRange y 
                              then      makeLine '|' ' ' '║' numCols Nothing   Nothing             Nothing                   mWall   brickYPositions bricklength
                         else if yWallBallRange y && yWallRange y
                              then      makeLine '|' ' ' '║' numCols Nothing   Nothing             Nothing                   mWall   brickYPositions bricklength
                              else      makeLine '|' ' ' ' ' numCols Nothing   Nothing             Nothing                   Nothing brickYPositions bricklength
            
        -- | Conditions to choose which part of the Smart Walls needs to be painted

            yWallBallRange y  = abs (ballY - y) <= 1

            yWallEnds y       = y == div (numRows - 2 - wallHeight) 2 - 1 || y == div (numRows - 2 + wallHeight) 2 + 1

            yWallUpperPart y  = y < div (numRows - 2) 2 - (div wallHeight 2 - 1)  && ballY < div (numRows - 2) 2 - (div wallHeight 2 - 1)

            yWallLowerPart y  = y > div (numRows - 2) 2 +  div wallHeight 2 - 1  && ballY > div (numRows - 2) 2 +  div wallHeight 2 - 1

            yWallRange y      = abs (y - div (numRows - 2) 2) <= div wallHeight 2

            celebratrionCartoon = "\n                /`_.----._"
                                    ++"\n              .¨ _,=<'¨=. ¨,/|   Hey you did great."
                                    ++"\n             /,-'    ¨=.`.   (   You're almost as good as Sulley!"
                                    ++"\n            //         ` |    `"
                                    ++"\n           /,    _.,.   |      `    (|"
                                    ++"\n         ,¨ |   ,`'v/', |       `  _)("
                                    ++"\n        /   |   !>(¨)<|/         ` c_ `"
                                    ++"\n     _-/     `  '=,Z``7           . C. `       ¡CONGRATULATIONS!"         
                                    ++"\n _,-¨ V  /    '-._,>*¨     `      |   ` `"
                                    ++"\n `  <¨|  )` __ __ ____ _    Y     |    ` `"
                                    ++"\n  ` ` |   >._____________.< |     ¨-.   ` `"
                                    ++"\n   ` `|   ` `/`/`/`/`/`/ /' /    =_  '-._) `"
                                    ++"\n    ` (    `            /         |¨*=,_   /"
                                    ++"\n     ` `    `_/`/`/`/`_/         /      ¨¨¨"
                                    ++"\n     _).^     ¨******¨          /"
                                    ++"\n    (()!|`                     /      Your Score: " ++ show points ++ " points"
                                    ++"\n     *==¨ ¨,                 ,¨"
                                    ++"\n            ¨,_            ,¨"
                                    ++"\n               `¨*<==> ,=*¨"
                                    ++"\n                ` ` / /"
                                    ++"\n            _____>_V /"
                                    ++"\n           f  .-----¨"
                                    ++"\n           |  `    ` `                   Next Level - Press SPACE"
                                    ++"\n           |   `    ` '-."
                                    ++"\n           J    `    `   `"
                                    ++"\n          (  ` ` ` _.-J   )"
                                    ++"\n           `V)V)=*.','  ,'"
                                    ++"\n               (V(V)(V)/"

            {-
                                    "                        .-."
                                ++"\n                _.--¨¨¨¨.o/         .-.-._"
                                ++"\n             __'   .¨¨¨; {        _J ,__  `.       Level Complete"
                                ++"\n            ; o`.-.`._.'J;       ; /  `- /  ;"
                                ++"\n            `--i`¨. `¨ .';       `._ __.'   |     ¡CONGRATULATIONS!"
                                ++"\n                `  `¨¨¨   `         `;      :"
                                ++"\n                 `.¨-.     ;     ____/     /     Your Score: " ++ show points ++ " points"
                                ++"\n                   `-.`     `-.-'    `¨-..'"
                                ++"\n     ___              `;__.-'¨           `."
                                ++"\n  .-{_  `--._         /.-¨                 `-."
                                ++"\n /    ¨¨T    ¨¨---...'  _.-¨¨   ¨¨¨-.         `."
                                ++"\n;       /                 __.-¨¨.    `.         `,             _.."
                                ++"\n `     /            __.-¨¨       '.    `          `.,__      .'L' }"
                                ++"\n  `---¨`-.__    __.¨    .-.       j     `.         :   `.  .' ,' /"
                                ++"\n            ¨¨¨¨       /   `     :        `.       |     F' `   ;"
                                ++"\n                      ;     `-._,L_,-¨¨-.   `-,    ;     `   ; /"
                                ++"\n                       `.       |        `-._  `.__/_        `/"
                                ++"\n                         `     _;            `  _.'  `-.     /"
                                ++"\n                          `---¨ `.___,,      ;¨¨        `  .'"
                                ++"\n                                    _/       ;           `¨"
                                ++"\n      Bring me                   .-¨     _,-' "
                                ++"\n     more bricks!               {       ¨¨;            Next Level - Press SPACE"
                                ++"\n                                 ;-.____.'`."
                                ++"\n      I am not done yet!          `.  ` '.  :"
                                ++"\n                                    `  : : /"
                                ++"\n                                     `':/ `"-}
