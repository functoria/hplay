module GrahamScan (Point, convexHull)
where

    import Data.List           


    --Exercise 9
    data Direction = R | S | L deriving (Eq, Show)

    --Exercises 10, 11, 12
    type Point = (Double, Double)  
    
    xProduct :: Point -> Point -> Point -> Double
    xProduct (x1, y1) (x2, y2) (x3, y3) = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)
  
    turnX :: Point -> Point -> Point -> Direction
    turnX p1 p2 p3 | xp > 0 = L
                   | xp == 0 = S
                   | xp < 0 = R
                   where
                        xp = xProduct p1 p2 p3 

    comparePoints :: Point -> Point -> Ordering
    comparePoints (x1, y1) (x2, y2) | y1 < y2 = LT
                                    | y1 > y2 = GT
                                    | y1 == y2 = compare x1 x2  

    peekLeft :: Point -> Point -> Point
    peekLeft p q = case comparePoints p q of
                        LT -> p
                        _  -> q

   
    bottomLeft :: [Point] -> Point
    bottomLeft = foldr peekLeft (1 / 0, 1 / 0)  

    compareAngles :: Point -> Point -> Point -> Ordering 
    compareAngles _ _ _ = LT   -- <--- TODO


    grahamSort :: Point -> [Point] -> [Point]
    grahamSort p = sortBy (compareAngles p)         

    grahamScan :: [Point] -> [Point] -> [Point]
    grahamScan [p] (q:ps) | turnX pLast p q == S = [pLast, p] 
                            where pLast = last ps
    grahamScan (p:ps) (q:r:others) = case turnX p q r of
                                L -> grahamScan (q:p:ps) (r:others)
                                S -> grahamScan (p:ps) (r:others)
                                R -> grahamScan ps (p:r:others)
    grahamScan ps [p] = p:ps

    convexHull :: [Point] -> [Point]
    convexHull ps | length ps <= 3 = ps
                  | otherwise = (reverse . grahamScan [p0]) points
                    where
                        p0 = bottomLeft ps
                        rest = filter (/= p0) ps
                        points = grahamSort p0 rest

                        
                               