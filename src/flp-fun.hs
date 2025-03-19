-- ============================================================
--  Project: FLP - Decision Tree Classifier
--  Author: Dinara Garipova
--  Login: xgarip00
--  Year: 2024/25
--  Description: Decision tree definition and tree building from indented text format.
-- ============================================================

import System.Environment (getArgs)
import Control.Exception (catch, IOException)
import Data.List (sortOn)
import Numeric (showGFloat)

-- ===== DATA TYPES =====
data Point = Leaf String | Node Int Double deriving (Show, Eq)
data Tree = Empty | Tree Point Tree Tree deriving (Show, Eq)

-- ===== CLEANING AND PARSING =====

-- Remove all spaces except leading spaces (indentation)
cleanLine :: String -> String
cleanLine line =
    let leadingSpaces = takeWhile (== ' ') line             
        rest = dropWhile (== ' ') line                     
        restNoSpaces = filter (/= ' ') rest               
    in leadingSpaces ++ restNoSpaces                     

-- Count leading spaces in a line
countSpaces :: String -> Int
countSpaces = length . takeWhile (== ' ')

-- Split line by ':' and ',' (without keeping them)
splitSpecial :: String -> [String]
splitSpecial [] = []
splitSpecial s =
    let (word, rest) = break (\c -> c == ':' || c == ',') s
    in case rest of
        [] -> [word] 
        (_:xs) -> word : splitSpecial xs 

-- Remove all spaces in line (used after counting indentation)
removeAllSpaces :: String -> String
removeAllSpaces = filter (/= ' ')

-- ===== LINES PROCESS FUNCTION =====

processLines :: [String] -> IO ()
processLines linesList = do
    let spacesList = map countSpaces linesList
    let cleanedLines = map removeAllSpaces linesList
    let wordsList = map splitSpecial cleanedLines

    putStrLn "Indexed lines with indentation and tokens:"
    printIndexedLines spacesList wordsList

    if null spacesList || head spacesList /= 0
        then error "Error: Root node must start without indentation."
        else putStrLn "Indentation OK, starting tree creation..."

    let indexedNodes = zip (zip [0..] spacesList) wordsList
    let (tree, _) = buildTree indexedNodes (head indexedNodes, tail indexedNodes) (-1)

    putStrLn "Tree created:"
    printTree tree 0

-- Pretty print indexed lines with indentation and tokens
printIndexedLines :: [Int] -> [[String]] -> IO ()
printIndexedLines spacesList wordsList = 
    mapM_ putStrLn [ show idx ++ ": " ++ show spaces ++ " " ++ show tokens | (idx, spaces, tokens) <- zip3 [0..] spacesList wordsList ]


-- ===== ERROR HANDLER =====

errorHandler :: IOException -> IO ()
errorHandler _ = putStrLn "Error: File not found or cannot be read!"

-- ===== TREE BUILDING FUNCTIONS =====

-- Find two children for a node based on indentation and index, considering an optional sibling index
findChildren :: Int -> Int -> Int -> [((Int, Int), [String])] -> [((Int, Int), [String])]
findChildren parentIdx childIndent siblingIdx allNodes =
    let children = filter (\((idx, sp), _) -> sp == childIndent && idx > parentIdx && (siblingIdx == -1 || idx < siblingIdx)  ) allNodes
    in case children of
        [c1, c2] -> [c1, c2] 
        _ -> error $ "Error: Expected 2 children at indentation level " ++ show childIndent ++ ", found " ++ show (length children)



-- Recursively build tree from list of nodes with (index, indent, tokens)
buildTree :: [((Int, Int), [String])] -> (((Int, Int), [String]), [((Int, Int), [String])]) -> Int -> (Tree, [((Int, Int), [String])])
buildTree allNodes (((idx, indent), tokens), rest) siblingIdx
    | head tokens == "Leaf" = (Tree (Leaf (tokens !! 1)) Empty Empty, rest) 
    | head tokens == "Node" =
        if length tokens < 3
            then error $ "Invalid Node definition at index " ++ show idx
            else
                let index = read (tokens !! 1) :: Int
                    threshold = read (tokens !! 2) :: Double
                    children = findChildren idx (indent + 2) siblingIdx allNodes
                in case children of
                    [leftChild, rightChild] ->
                        let (leftSubtree, remainingNodes1) = buildTree allNodes (leftChild, rest) (fst (fst rightChild))
                            (rightSubtree, remainingNodes2) = buildTree allNodes (rightChild, remainingNodes1) siblingIdx
                        in (Tree (Node index threshold) leftSubtree rightSubtree, remainingNodes2)
                    [] -> (Empty, rest)

                    _ -> error $ "Cannot find two children for node at index " ++ show idx

-- Function for beautifully indented tree output
printTree :: Tree -> Int -> IO ()
printTree Empty _ = return () 
printTree (Tree point left right) indent = do
    putStrLn $ replicate indent ' ' ++ formatPoint point  
    printTree left (indent + 2)   
    printTree right (indent + 2)  

-- Format the Node and Leaf as desired
formatPoint :: Point -> String
formatPoint (Node i t) = "Node: " ++ show i ++ ", " ++ showGFloat Nothing t "" -- Node: X, Y
formatPoint (Leaf label) = "Leaf: " ++ label  -- Leaf: X



-- ===== MAIN FUNCTION =====

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
