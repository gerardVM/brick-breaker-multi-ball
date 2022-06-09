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
                                (multiplier  st ) 
                                (status      st )
                                (points      st )
                                (title       env)
                                (maxBalls    env)

{-|
  Definition of how each line is going to be rendered according to what is there. Oprions are:
  1) Nothing / 2) Just bricks / 3) Just the ball / 4) Just the base / 5) The ball and bricks /
  6) The ball and the base / (The base and bricks is not an option)
  7) Ball and bricks combined with Multipliers
-}

makeLine :: Char 
         -> Char 
         -> Int 
         -> Maybe [Object]
         -> Maybe Object
         -> [Object] 
         -> Int
         -> Maybe [Object]
         -> String

makeLine eCh iCh i mBalls mBase bricks bricklength mMultiplier =
    let positions  = [0 .. i]
        ball       = '●'
        multiplier = '☺'
        base       = '═'
        pickXBall       (Ball       (xPos, yPos)) = xPos
        pickXMultiplier (Multiplier (xPos, yPos)) = xPos
        renderPixel x = case mBalls of
                        Nothing    -> case mBase of 
                                          Nothing           -> case mMultiplier of 
                                                                    Nothing          ->                                                    printBlock x iCh
                                                                    Just multipliers -> if x `elem` (map pickXMultiplier multipliers) then multiplier            
                                                                                                                                      else printBlock x iCh
                                          Just (Base bl bx) -> case mMultiplier of 
                                                                    Nothing          -> if x `elem` [bx..(bx+bl)]                     then base 
                                                                                                                                      else iCh

                                                                    Just multipliers -> if x `elem` [bx..(bx+bl)]                     then base 
                                                                                   else if x `elem` (map pickXMultiplier multipliers) then multiplier
                                                                                                                                      else iCh
                        Just balls -> case mBase of 
                                          Nothing           -> case mMultiplier of 
                                                                    Nothing          -> if x `elem` (map pickXBall balls)             then ball 
                                                                                                                                      else printBlock x iCh

                                                                    Just multipliers -> if x `elem` (map pickXBall balls)             then ball 
                                                                                   else if x `elem` (map pickXMultiplier multipliers) then multiplier            
                                                                                                                                      else printBlock x iCh
                                          Just (Base bl bx) -> case mMultiplier of 
                                                                    Nothing          -> if x `elem` (map pickXBall balls)             then ball 
                                                                                   else if x `elem` [bx..(bx+bl)]                     then base
                                                                                                                                      else iCh

                                                                    Just multipliers -> if x `elem` (map pickXBall balls)             then ball 
                                                                                   else if x `elem` [bx..(bx+bl)]                     then base
                                                                                   else if x `elem` (map pickXMultiplier multipliers) then multiplier
                                                                                                                                      else iCh                                                                               
     in [eCh] ++ map renderPixel positions ++ [eCh]
     
     -- Finding if a pixel should belong to a brick considering its position and length
     -- If True, we paint it according to the life of the brick
     
     where brickXPositions = map (fst . brickPosition) bricks
           printBlock x ch = if x `elem` foldl (\u v  -> u ++ [v..(v+bricklength-1)]) [] brickXPositions
                             then if (life $ pixelOwner x) > 1 then '█' else if (life $ pixelOwner x) > 0 then '▓' else '░'
                             else ch
           pixelOwner x    = head $ filter (\u -> x - fst (brickPosition u) < bricklength
                                               && x - fst (brickPosition u) >= 0 ) bricks

-- | Painting lines depending on the Y coordinate of the different elements

makeBox :: (Int, Int) 
        -> Int 
        -> Int 
        -> [(Int, Int)]
        -> Int 
        -> [Object] 
        -> [Object]
        -> GameStatus
        -> Int
        -> String
        -> Int
        -> String
makeBox (numCols, numRows) baseL baseX ballPositions bricklength bricks multipliers status points title maxBalls =
    unlines 
        ([" "] ++ [indent ++ take (div (numCols - length title + 4) 2) (repeat ' ') ++ title] 
               ++ [indent ++ if bricks /= [] then "Score: " ++ show points else []]  
        ++ case status of 
              LevelComplete -> [ celebratrionCartoon ]
              _             -> [ indent ++ makeLine '▄' '▄' numCols Nothing Nothing [] bricklength Nothing ]
                              ++   mappedPositions          
                            ++ [ indent ++ makeLine '▀' '▀' numCols Nothing Nothing [] bricklength Nothing ]
                            ++ [ indent ++ "Status: " ++ if ballPositions == [] then gameOver else show status    
                                        ++ take   displaySpace                    ( repeat ' ' )
                                        ++ take   numberOfBalls                   ( repeat '●' )
                                        ++ take ( maxDisplay - numberOfBalls + 3) ( repeat '○' )
                               ] 
                         
                           -- Render menu according to status
                         
                            ++ [ case status of

                                     Stopped       -> indent ++ "Press: (R) to Restart              "    ++ "\n" ++ "\n"    -- "\n" Necessary for inline space coherence

                                     Paused        -> indent ++ "Press (P) to keep playing / (SPACE) Auto Mode"  ++ "\n"
                                                   ++ indent ++ "(A) Move Left / (D) Move Right"                 ++ "\n"

                                     Playing       -> indent ++ "(P) Pause / (Q) Stop / (SPACE) Auto Mode"       ++ "\n"
                                                   ++ indent ++ "(A) Move Left / (D) Move Right"                 ++ "\n"

                                     Starting      -> indent ++ "Press: (S) to Play / (R) to rearrange Bricks"   ++ "\n"
                                                   ++ indent ++ "(A) Move Left / (D) Move Right"                 ++ "\n"

                                     Auto          -> indent ++ "(P) Pause / (Q) Stop"                           ++ "\n"
                                                   ++ indent ++ "Pause the game to remove Auto Mode"             ++ "\n"

                                     _             ->                                                       "\n" ++ "\n"    -- "\n" Necessary for inline space coherence
                               ]

                           -- Uncomment these lines for debugging purposes 

--                              ++ [  indent ++ "Balls:       " ++ show ( take 9 ballPositions ) ]
--
--                              ++ [  indent ++ "Multipliers: " ++ show (
--                                                                       let pickPosition []     = []            
--                                                                           pickPosition (x:xs) = multiplierPosition x : pickPosition xs  
--                                                                        in take 9 $ pickPosition multipliers
--                                                                      ) 
--                                 ]
--
--                              ++ [  indent ++ "BaseX: " ++ show (baseX + div baseL 2) ]   

        )

    where   numberOfBalls   = ( min ( maxDisplay + 3 ) $ div (length ballPositions) (div maxBalls maxDisplay - 1) + 1 )
            maxDisplay      = div numCols 4
            displaySpace    = numCols - length ("Status: " ++ show status) - maxDisplay
            gameOver        = "***** GAME OVER *****"
            indent          = take 20 $ repeat ' '
            mappedPositions = map lineMaker [0 .. numRows]

         -- Painting lines depending on the Y of the different elements
            
            lineMaker y =
              let brickYPositions = filter ((==) y . snd . brickPosition) bricks
                  balls           = map Ball $ filter ((==) y . snd) ballPositions
                  multipliersInY  = filter ((==) y . snd . multiplierPosition) multipliers
               in if balls /= []

                  then if y == numRows - 1

                       then if (multipliersInY /= [])

                            then indent ++ makeLine '█' ' ' numCols (Just balls) (Just (Base baseL baseX)) brickYPositions bricklength (Just multipliersInY)
                    
                            else indent ++ makeLine '█' ' ' numCols (Just balls) (Just (Base baseL baseX)) brickYPositions bricklength Nothing

                       else if multipliersInY /= []

                       then      indent ++ makeLine '█' ' ' numCols (Just balls) Nothing                   brickYPositions bricklength (Just multipliersInY)

                       else      indent ++ makeLine '█' ' ' numCols (Just balls) Nothing                   brickYPositions bricklength Nothing

                       

                  else if y == numRows - 1

                       then if (multipliersInY /= [])

                            then indent ++ makeLine '█' ' ' numCols Nothing (Just (Base baseL baseX)) brickYPositions bricklength (Just multipliersInY)
                    
                            else indent ++ makeLine '█' ' ' numCols Nothing (Just (Base baseL baseX)) brickYPositions bricklength Nothing

                       else if multipliersInY /= []

                       then      indent ++ makeLine '█' ' ' numCols Nothing Nothing                   brickYPositions bricklength (Just multipliersInY)

                       else      indent ++ makeLine '█' ' ' numCols Nothing Nothing                   brickYPositions bricklength Nothing



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
