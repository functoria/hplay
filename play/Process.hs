
import System.Environment (getArgs)
import Data.List

doProcess processor inFile outFile = do
    input <- readFile inFile
    writeFile outFile (processor input)

main = mainWith processor where
    mainWith processor = do
        args <- getArgs
        case args of
            [input, output] -> doProcess processor input output
            _ -> putStrLn "Error: exactly two arguments expected"
        
    processor = fixLines

    fixLines :: String -> String
    fixLines = unlines . splitLines 
    
    splitLines [] = []
    splitLines cs = 
        let (pre, suf) = break isLineTerminator cs 
        in pre : case suf of
                    ('\r':'\n':rest) -> splitLines rest
                    ('\r':rest)      -> splitLines rest
                    ('\n':rest)      -> splitLines rest
                    _                -> []
    isLineTerminator c = c == '\r' || c == '\n'