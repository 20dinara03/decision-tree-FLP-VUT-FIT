-- ============================================================
--  Project: FLP - Decision Tree Classifier
--  Author: Dinara Garipova
--  Login: xgarip00
--  Year: 2024/25
--  Description: Decision tree definition and classification 
--               from an indented text format.
-- ============================================================

import System.Environment (getArgs)
import Control.Exception (catch, IOException)
import Numeric (showGFloat)

-- ================= DATA TYPES ==================

-- Representation of a decision tree node or leaf
data Point = Leaf String | Node Int Double deriving (Show, Eq)

-- Decision tree structure
data Tree = Empty | Tree Point Tree Tree deriving (Show, Eq)

-- ================= UTILITY FUNCTIONS ==================

-- Removes all spaces except leading spaces (preserves indentation)
cleanLine :: String -> String
cleanLine line =
    let leadingSpaces = takeWhile (== ' ') line
        rest = dropWhile (== ' ') line
        restNoSpaces = filter (/= ' ') rest
    in leadingSpaces ++ restNoSpaces

-- Counts leading spaces in a line (indentation level)
countSpaces :: String -> Int
countSpaces = length . takeWhile (== ' ')

-- Splits a line by ':' and ',' without keeping them
splitSpecial :: String -> [String]
splitSpecial [] = []
splitSpecial s =
    let (word, rest) = break (\c -> c == ':' || c == ',') s
    in case rest of
        [] -> [word]
        (_:xs) -> word : splitSpecial xs

-- Removes all spaces in a line (used after counting indentation)
removeAllSpaces :: String -> String
removeAllSpaces = filter (/= ' ')

-- ================= TREE CONSTRUCTION ==================

-- Finds two children for a given node based on indentation level
findChildren :: Int -> Int -> Int -> [((Int, Int), [String])] -> [((Int, Int), [String])]
findChildren parentIdx childIndent siblingIdx allNodes =
    let children = filter (\((idx, sp), _) -> sp == childIndent && idx > parentIdx && (siblingIdx == -1 || idx < siblingIdx)) allNodes
    in case children of
        [c1, c2] -> [c1, c2]
        _ -> error $ "Error: Expected 2 children at indentation level " ++ show childIndent ++ ", found " ++ show (length children)

-- Recursively builds a decision tree from a list of nodes
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

-- ================= TREE PRINTING ==================

-- Formats a tree node or leaf as a readable string
formatPoint :: Point -> String
formatPoint (Node i t) = "Node: " ++ show i ++ ", " ++ showGFloat Nothing t ""
formatPoint (Leaf label) = "Leaf: " ++ label

-- Prints the tree with indentation for readability
printTree :: Tree -> Int -> IO ()
printTree Empty _ = return ()
printTree (Tree point left right) indent = do
    putStrLn $ replicate indent ' ' ++ formatPoint point
    printTree left (indent + 2)
    printTree right (indent + 2)

-- ================= CLASSIFICATION ==================

-- Classifies a single input using the decision tree
classify :: [Double] -> Tree -> String
classify _ Empty = "Unknown"
classify _ (Tree (Leaf label) _ _) = label
classify input (Tree (Node index threshold) left right) =
    if (input !! index) <= threshold
        then classify input left
        else classify input right

-- Parses a line of input into a list of doubles
parseInput :: String -> [Double]
parseInput line = map read (splitSpecial line)

-- ================= ERROR HANDLING ==================

-- Handles file read errors
errorHandler :: IOException -> IO ()
errorHandler _ = putStrLn "Error: File not found or cannot be read!"

-- ================= MAIN FUNCTION ==================

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-1", treeFile, dataFile] -> do
            catch (do
                -- Read and parse the tree file
                treeContent <- readFile treeFile
                let treeLines = map (cleanLine . filter (/= '\r')) (lines treeContent)
                let indexedNodes = zip (zip [0..] (map countSpaces treeLines)) (map splitSpecial (map removeAllSpaces treeLines))
                let (tree, _) = buildTree indexedNodes (head indexedNodes, tail indexedNodes) (-1)

                -- Read and classify the input data
                dataContent <- readFile dataFile
                let newInputs = map parseInput (lines dataContent)
                let results = map (`classify` tree) newInputs

                -- Output classification results
                mapM_ putStrLn results
                )
                errorHandler
        _ -> putStrLn "Usage: ./flp-fun -1 <tree_file> <data_file>"
