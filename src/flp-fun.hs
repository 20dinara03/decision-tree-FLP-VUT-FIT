-- ============================================================
--  Project: FLP - Decision Tree Classifier
--  Author: Dinara Garipova
--  Login: xgarip00
--  Year: 2024/25
--  Description: Decision tree definition and pretty-printing.
-- ============================================================

import System.Environment (getArgs)
import Control.Exception (catch, IOException)

-- Node: either a leaf (with class) or a regular node (with feature index and threshold)
data Point = Leaf String | Node Int Float deriving (Show, Eq)

-- Tree: Node (Point) + left and right subtree (recursively Strom)
data Strom = Strom Point (Maybe Strom) (Maybe Strom) deriving (Show, Eq)

-- Function to remove all spaces except those at the beginning of the string
cleanLine :: String -> String
cleanLine line =
    let leadingSpaces = takeWhile (== ' ') line             
        rest = dropWhile (== ' ') line                     
        restNoSpaces = filter (/= ' ') rest               
    in leadingSpaces ++ restNoSpaces                     


-- Function for computing spaces on at the head of line
countSpaces :: String -> Int
countSpaces = length . takeWhile (== ' ')

-- Error function
errorHandler :: IOException -> IO ()
errorHandler _ = putStrLn "Error: File not found or cannot be read!"

-- Function to process list of lines
processLines :: [String] -> IO ()
processLines linesList = do
    let spacesList = map countSpaces linesList  -- counting spaces
    putStrLn "Indentation levels (number of leading spaces):"
    print spacesList

    let cleanedLines = map removeAllSpaces linesList  -- delete all the spaces
    putStrLn "Cleaned lines (without spaces):"
    print cleanedLines

    let wordsList = map splitSpecial cleanedLines  -- breaking down the strings into words
    putStrLn "Split lines into tokens:"
    print wordsList

    -- Check the first element for 0 spaces
    if head spacesList /= 0
        then error "Error: Root node must start without indentation."
        else putStrLn "Indentation OK, starting tree creation..."

-- Function for deliting all spaces in line
removeAllSpaces :: String -> String
removeAllSpaces = filter (/= ' ')

--Function for splitting line to words
splitSpecial :: String -> [String]
splitSpecial [] = []
splitSpecial s =
    let (word, rest) = break (\c -> c == ':' || c == ',') s
    in case rest of
        [] -> [word] 
        (_:xs) -> word : splitSpecial xs 



-- Function to process each line and each char
processLine :: String -> IO ()
processLine line = mapM_ (\symbol -> do
    putStrLn $ "Symbol: " ++ [symbol]
    ) line


-- Main function to handle CLI args and parsing
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-1", treeFile] -> do
            catch (do
                content <- readFile treeFile
                let ls = map (cleanLine . filter (/= '\r')) (lines content)
                processLines ls
                )
                errorHandler
        _ -> do
            putStrLn "Usage: ./flp-fun -1 <tree_file>"