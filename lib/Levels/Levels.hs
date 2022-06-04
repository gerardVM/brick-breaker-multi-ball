module Levels.Levels where

import Animation.Env (Env(..))
import Animation.Type (Object(..))

level_1 :: Env -> [Object]
level_1 (Env _ _ (width, height) _ _ bricklength _ _ _ _ _) = let holeInTheMiddle = filter ( (<) (div width 45) . abs . (-) (div width 2) ) [ 0, bricklength .. width ]
                                                                  playableHeight  = filter (/= (div height 2 - 1)) [ 4 .. (height-4) ]
                                                                  line xs y       = xs ++ ( take width $ map ( \i -> Brick ( i, y ) 2 ) holeInTheMiddle )
                                                               in map ( \i -> Brick ( i, 3 ) 2 ) (filter ( (<) (div width 45) . abs . (-) (div width 2) ) [ 0, 2 * bricklength .. width ])
                                                                  ++ foldl line [] playableHeight