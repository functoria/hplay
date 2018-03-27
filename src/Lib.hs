module Lib
    ( someFunc, lastButOne, drop_
    ) where

drop_ :: Show a => Int -> [a] -> [a]
drop_ n xs = if n <= 0 || null xs then xs else drop_ (n-1) (tail xs)

lastButOne :: Show a => [a] -> a
lastButOne xs = head $ drop (length xs - 2) $ xs 
        
someFunc :: IO ()
someFunc =
        interact wordCount  
        where
            showLength = show . length
            wordCount input = "The given file has "
                                ++ (showLength . words $ input) ++ " words in "
                                ++ (showLength . lines $ input) ++ " lines and a total of "
                                ++ showLength input
                                ++ " characters.\n"
                                ++ (showLength . drop_ 100 $ input)
                                ++ (show . lastButOne $ "ABC")

    
                                --TODO make this more efficient for large files
                                {-   Multiline comment -}
                                --- 


