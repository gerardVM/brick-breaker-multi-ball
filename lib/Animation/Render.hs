module Animation.Render where

import Control.Monad.Trans.State.Strict (get)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Class (lift)
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(BlockBuffering))

import Animation.Env (Env(..))
import Animation.State (St(..))
import Animation.Type (Animation,Object(..), GameStatus(..))

render :: Animation Env St ()
render = do
    lift $ lift $ hSetBuffering stdout $ BlockBuffering (Just 50)
    val <- renderVal
    lift $ lift $ putStrLn val
    lift $ lift $ hFlush stdout

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
         -> Char 
         -> Int 
         -> Maybe Int
         -> Maybe Object 
         -> Maybe Object
         -> Maybe Object
         -> [Object] 
         -> Int
         -> String

makeLine eCh iCh wCh0 wCh1 i mWG mBall mBase mWall bricks bricklength =
    let positions = [0 .. i]
        ball = '●'
        renderPixel x =
            case mBall of
                Nothing -> case mBase of 
                               Nothing           -> case mWG of 
                                                        Just wG -> if x == wG || x == i - wG 
                                                                   then case mWall of
                                                                            Just ( Wall (Left  wx) ) -> if x == wx then printBlock x wCh1 else printBlock x wCh0
                                                                            Just ( Wall (Right wx) ) -> if x == wx then printBlock x wCh1 else printBlock x wCh0
                                                                            _                        -> printBlock x wCh0
                                                                   else printBlock x iCh
                                                        Nothing -> case mWall of
                                                                            Just ( Wall (Left  wx) ) -> if x == wx then printBlock x wCh1 else printBlock x iCh
                                                                            Just ( Wall (Right wx) ) -> if x == wx then printBlock x wCh1 else printBlock x iCh
                                                                            _                        -> printBlock x iCh
                               Just (Base bl bx) -> if x `elem` [bx..(bx+bl)] then '═' else iCh
                                 
                Just (Ball b) -> case mBase of 
                               Nothing           -> case mWG of 
                                                        Just wG -> if x == b then ball 
                                                              else if x == wG || x == i - wG
                                                                   then case mWall of
                                                                            Just ( Wall (Left  wx) ) -> if x == wx then printBlock x wCh1 else printBlock x wCh0
                                                                            Just ( Wall (Right wx) ) -> if x == wx then printBlock x wCh1 else printBlock x wCh0
                                                                            _                        -> printBlock x wCh0
                                                                   else printBlock x iCh
                                                        Nothing -> case mWall of
                                                                            Just ( Wall (Left  wx) ) -> if x == b  then ball 
                                                                                                   else if x == wx then printBlock x wCh1 
                                                                                                                   else printBlock x iCh
                                                                            Just ( Wall (Right wx) ) -> if x == b  then ball 
                                                                                                   else if x == wx then printBlock x wCh1 
                                                                                                                   else printBlock x iCh
                                                                            _                        -> if x == b  then ball else printBlock x iCh
                               Just (Base bl bx) -> if x == b then ball 
                                                    else if x `elem` [bx..(bx+bl)] then '═' 
                                                    else iCh
                                
     in [eCh] ++ map renderPixel positions ++ [eCh]
     
     -- | Finding if a pixel should belong to a brick considering its position and length
     -- | If True, we paint it according to the life of the brick
     
     where brickXPositions = map (fst . brickPosition) bricks
           printBlock x ch = if x `elem` foldl (\u v  -> u ++ [v..(v+bricklength-1)]) [] brickXPositions
                             then if (life $ pixelOwner x) > 0 then '▒' else '░'
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
        ([" "] ++ [indent ++ take (div (numCols - length title + 4) 2) (repeat ' ') ++ title] ++ [" "] --  
        ++ case status of 
              LevelComplete -> [ celebratrionCartoon ]
              _             -> [ indent ++ makeLine '▄' '▄' ' ' ' ' numCols Nothing Nothing Nothing Nothing [] bricklength ]
                              ++   mappedPositions          
                            ++ [ indent ++ makeLine '▀' '▀' ' ' ' ' numCols Nothing Nothing Nothing Nothing [] bricklength ]
                            ++ [ indent ++ "Status: " ++ show status
                              ++ if ballY /= numRows then   " | Score: " ++ show points 
                                 else  " | ***** GAME OVER ***** | Your Score is " ++ show points 
                               ]
                         
                           -- | Render menu according to status
                         
                            ++ [ case status of

                                     Stopped       -> indent ++ "Press: (R) to Restart              "    ++ "\n" ++ "\n" -- ^ "\n" Necessary for inline space coherence

                                     Paused        -> indent ++ "Press (P) to keep playing"                      ++ "\n"
                                                   ++ indent ++ "Player 1: (A) Move Left       / (D) Move Right" ++ "\n"
                                                   ++ indent ++ "Player 2: (J) Left Smart Wall / (L) Right Smart Wall"

                                     Playing       -> indent ++ "(P) Pause / (Q) Stop"                           ++ "\n"
                                                   ++ indent ++ "Player 1: (A) Move Left / (D) Move Right"       ++ "\n"
                                                   ++ indent ++ "Player 2: (J) Left Wall / (L) Right Wall"

                                     Starting      -> indent ++ "Press: (S) to Play / (R) to rearrange Bricks"   ++ "\n"
                                                   ++ indent ++ "Player 1: (A) Move Left       / (D) Move Right" ++ "\n"
                                                   ++ indent ++ "Player 2: (J) Left Smart Wall / (L) Right Smart Wall"
                                     _             ->                                                       "\n" ++ "\n" -- ^ "\n" Necessary for inline space coherence
                               ]

                           -- | Uncomment these lines for debugging purposes 

--                            ++ [  "BaseX: " ++ show (baseX + div baseL 2) 
--                               ++ " | Ball: (" ++ show ballX ++ "," ++ show ballY ++ ")"
--                               ++ " | BallOverBase: "   ++ show (ballX >= baseX && (ballX <= (baseX + baseL)))
--                               ]
        )

    where   indent          = take 20 $ repeat ' '
            mappedPositions = map lineMaker [0 .. numRows]

         -- | Painting lines depending on the Y of the different elements
            
            lineMaker y =
              let brickYPositions  = filter ((==) y . snd . brickPosition) bricks

         -- | Conditions to choose which part of the Smart Walls needs to be painted

                  yWallBallRange y = abs (y - ballY)        <=   1 

                  yWallRange y     = abs (fromWallCenter y) <    div wallHeight 2 

                  yWallEnds y      = abs (fromWallCenter y) ==   div wallHeight 2

                  yWallUpperPart y = fromWallCenter y       == - div wallHeight 2 + 1
                                  && fromWallCenter ballY   <= - div wallHeight 2 + 1

                  yWallLowerPart y = fromWallCenter y       ==   div wallHeight 2 - 1
                                  && fromWallCenter ballY   >=   div wallHeight 2 - 1

                  fromWallCenter x = x - div (numRows - 2) 2          

                  eCh1 y           = if fromWallCenter y < 0 then '╦' else '╩'

                  sCh y            = let lookLeft y  | y <  ballY = '╔' 
                                                     | y == ballY = '║'
                                                     | otherwise  = '╚'
                                         lookRight y | y <  ballY = '╗'
                                                     | y == ballY = '║'
                                                     | otherwise  = '╝'
                                      in case mWall of 
                                              Just (Wall (Left  xPos)) -> if ballX <= xPos then lookLeft y else lookRight y
                                              Just (Wall (Right xPos)) -> if ballX <= xPos then lookLeft y else lookRight y
                                              Nothing                  -> ' '

               in if y == ballY

                  then if y == numRows - 1
                       then indent ++ makeLine '█' ' ' ' ' ' '      numCols Nothing   (Just (Ball ballX)) (Just (Base baseL baseX)) Nothing brickYPositions bricklength

                       else if yWallEnds y
                       then indent ++ makeLine '█' ' ' '·' (eCh1 y) numCols (Just wG) (Just (Ball ballX)) Nothing                   mWall   brickYPositions bricklength

                       else if yWallRange y
                       then indent ++ makeLine '█' ' ' ' ' (sCh y ) numCols Nothing   (Just (Ball ballX)) Nothing                   mWall   brickYPositions bricklength

                       else indent ++ makeLine '█' ' ' ' ' ' '      numCols Nothing   (Just (Ball ballX)) Nothing                   Nothing brickYPositions bricklength

                  else if y == numRows - 1
                       then indent ++ makeLine '█' ' ' ' ' ' '      numCols Nothing   Nothing             (Just (Base baseL baseX)) Nothing brickYPositions bricklength

                       else if yWallEnds y 
                       then indent ++ makeLine '█' ' ' '·' (eCh1 y) numCols (Just wG) Nothing             Nothing                   mWall   brickYPositions bricklength

                       else if yWallUpperPart y || yWallLowerPart y || (yWallBallRange y && yWallRange y)
                       then indent ++ makeLine '█' ' ' ' ' (sCh y ) numCols Nothing   Nothing             Nothing                   mWall   brickYPositions bricklength

                       else indent ++ makeLine '█' ' ' ' ' ' '      numCols Nothing   Nothing             Nothing                   Nothing brickYPositions bricklength

            celebratrionCartoon = "\n"++indent++"                 /`_.----._"
                                    ++"\n"++indent++"              .¨ _,=<'¨=. ¨,/|   Hey you did great."
                                    ++"\n"++indent++"             /,-'    ¨=.`.   (   You're almost as good as Sulley!"
                                    ++"\n"++indent++"            //         ` |    `"
                                    ++"\n"++indent++"           /,    _.,.   |      `    (|"
                                    ++"\n"++indent++"         ,¨ |   ,`'v/', |       `  _)("
                                    ++"\n"++indent++"        /   |   !>(¨)<|/         ` c_ `"
                                    ++"\n"++indent++"     _-/     `  '=,Z``7           . C. `       ¡CONGRATULATIONS!"         
                                    ++"\n"++indent++" _,-¨ V  /    '-._,>*¨     `      |   ` `"
                                    ++"\n"++indent++" `  <¨|  )` __ __ ____ _    Y     |    ` `"
                                    ++"\n"++indent++"  ` ` |   >._____________.< |     ¨-.   ` `"
                                    ++"\n"++indent++"   ` `|   ` `/`/`/`/`/`/ /' /    =_  '-._) `"
                                    ++"\n"++indent++"    ` (    `            /         |¨*=,_   /"
                                    ++"\n"++indent++"     ` `    `_/`/`/`/`_/         /      ¨¨¨"
                                    ++"\n"++indent++"     _).^     ¨******¨          /"
                                    ++"\n"++indent++"    (()!|`                     /      Your Score: " ++ show points ++ " points"
                                    ++"\n"++indent++"     *==¨ ¨,                 ,¨"
                                    ++"\n"++indent++"            ¨,_            ,¨"
                                    ++"\n"++indent++"               `¨*<==> ,=*¨"
                                    ++"\n"++indent++"                ` ` / /"
                                    ++"\n"++indent++"            _____>_V /"
                                    ++"\n"++indent++"           f  .-----¨"
                                    ++"\n"++indent++"           |  `    ` `                   Next Level - Press SPACE"
                                    ++"\n"++indent++"           |   `    ` '-."
                                    ++"\n"++indent++"           J    `    `   `"
                                    ++"\n"++indent++"          (  ` ` ` _.-J   )"
                                    ++"\n"++indent++"           `V)V)=*.','  ,'"
                                    ++"\n"++indent++"               (V(V)(V)/"

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
