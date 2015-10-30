{-
    Program: matform.hs
    Author: Gerard Geer <github.com/gerard-geer>
    Description:
    A Haskell version of my Matrix Formatter. It simply allows you to
    enter some rows of numbers and provides you with that matrix
    formatted for use on Wolfram Alpha. 
    
    1 2 3 4
    6 4 1 6   ->   {{1,2,3,4},{6,4,1,6},{2,8,0,9}}
    2 8 0 9

    Plus the result is spit directly into the Clipboard!
-}

import System.Hclip

-- If a character is a space, we return a comma. Otherwise we return
-- the character.
commify :: Char -> Char
commify ' ' = ','
commify other = other

-- Replaces the spaces in a string with commas using map and commify.
replaceSpaces :: String -> String
replaceSpaces s = map commify s

-- Takes a sequence the user entered, and formats it as follows:
-- "{a,b,c,d,e,f,g..}"
constructBracketedSet :: String -> String
constructBracketedSet s = "{" ++ replaceSpaces s ++ "}"

-- Takes a list of Strings as the rows of a matrix and formats
-- them into a string that Wolfram likes.
formatMatrix :: [String] -> String
formatMatrix lst = 
    let rows = map constructBracketedSet lst
        str = unwords rows
    in constructBracketedSet str

-- Gets multiple lines of input from the user.
getLines :: Int -> IO [String]
getLines row = do
    putStr ("row " ++ (show row) ++ ": ")
    line <- getLine
    if null line then return []
    else do
        others <- getLines (row+1)
        return (line:others)
        
-- Main. Gets several lines of input and converts them.
main :: IO ()
main = do
    putStrLn "Enter each row of the matrix on a new line. Enter an empty line when done."
    putStrLn "Separate elements by spaces only."
    input <- getLines 0
    let result = (formatMatrix input)
    putStrLn "Result: "
    putStrLn result
    setClipboard result