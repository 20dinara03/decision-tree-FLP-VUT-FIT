-- ============================================================
--  Project: FLP - Decision Tree Classifier and Trainer
--  Author: Dinara Garipova
--  Login: xgarip00
--  Year: 2024/25
--  Description: Haskell implementation of a decision tree:
--               - Subtask 1: Parsing and using a tree for classification
--               - Subtask 2: Training a decision tree from labeled data 
--                 using Gini impurity (CART-inspired algorithm)
-- ============================================================

-- ===============IMPORTS=========================

import System.Environment (getArgs)
import Control.Exception (catch, IOException)
import Data.List (nub, sort, group)

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

-- Counts leading spaces
countSpaces :: String -> Int
countSpaces = length . takeWhile (== ' ')

-- Removes all spaces in a line
removeAllSpaces :: String -> String
removeAllSpaces = filter (/= ' ')

-- Splits a line by ':' and ',' 
splitSpecial :: String -> [String]
splitSpecial [] = []
splitSpecial s =
    let (word, rest) = break (\c -> c == ':' || c == ',') s
    in case rest of
        [] -> [word]
        (_:xs) -> word : splitSpecial xs

-- ================= TREE CONSTRUCTION ==================

-- Finds two children for a given node 
findChildren :: Int -> Int -> Int -> [((Int, Int), [String])] -> [((Int, Int), [String])]
findChildren parentIdx childIndent siblingIdx allNodes =
    let children = filter (\((idx, sp), _) -> sp == childIndent && idx > parentIdx && (siblingIdx == -1 || idx < siblingIdx)) allNodes
    in case children of
        [c1, c2] -> [c1, c2]
        _ -> error $ "Error: Expected 2 children at indentation level " ++ show childIndent ++ ", found " ++ show (length children)

-- Recursively builds a decision tree 
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
buildTree _ _ _ = error "Invalid tree format"

-- ================= CLASSIFICATION ==================

-- Parses a line of input into a list of doubles
parseInput :: String -> [Double]
parseInput line = map read (splitSpecial line)

-- Classifies a single input
classify :: [Double] -> Tree -> String
classify _ Empty = "Unknown"
classify _ (Tree (Leaf label) _ _) = label
classify input (Tree (Node index threshold) left right) =
    if (input !! index) <= threshold
        then classify input left
        else classify input right

-- ================= ERROR HANDLING ==================

-- Handles file read errors
errorHandler :: IOException -> IO ()
errorHandler _ = putStrLn "Error: File not found or cannot be read!"

-- ============= PARSING INPUT =======================

parseTrainLine :: String -> ([Double], String)
parseTrainLine line =
    let parts = splitSpecial line
        features = map read (init parts)   
        label = last parts                 
    in (features, label)

-- ================ TREE TRAINING =====================

train :: [([Double], String)] -> Tree
train [] = Empty
train rows
    | allSameClass rows = Tree (Leaf (snd (head rows))) Empty Empty
    | otherwise =
        let (bestAttr, bestThresh) = findBestSplit rows
            (leftRows, rightRows) = splitRows bestAttr bestThresh rows
            leftTree = train leftRows
            rightTree = train rightRows
        in Tree (Node bestAttr bestThresh) leftTree rightTree

-- Checking the condition that all data of the same class

allSameClass :: [([Double], String)] -> Bool
allSameClass [] = True
allSameClass ((_, c):xs) = all ((== c) . snd) xs

-- ================= FINDING SPLIT ==========================

findBestSplit :: [([Double], String)] -> (Int, Double)
findBestSplit rows =
    let numAttrs = length (fst (head rows))
        candidates = [(i, t) |
            i <- [0 .. numAttrs - 1],
            t <- nub . sort $ map (\(fs, _) -> fs !! i) rows]
        score (i, t) =
            let (left, right) = splitRows i t rows
                total = fromIntegral $ length rows
                giniLeft = gini (map snd left)
                giniRight = gini (map snd right)
                weighted = (fromIntegral (length left) / total) * giniLeft
                         + (fromIntegral (length right) / total) * giniRight
            in (weighted, (i, t))
    in snd $ minimum $ map score candidates

splitRows :: Int -> Double -> [([Double], String)] -> ([([Double], String)], [([Double], String)])
splitRows attrIndex threshold rows =
    (filter (\(fs, _) -> fs !! attrIndex <= threshold) rows,
     filter (\(fs, _) -> fs !! attrIndex > threshold) rows)

gini :: [String] -> Double
gini labels =
    let total = fromIntegral $ length labels
        groups = map length . group . sort $ labels
        probs = map (\n -> fromIntegral n / total) groups
    in 1.0 - sum (map (**2) probs)

-- ================= TREE PRINTING ==================

formatPoint :: Point -> String
formatPoint (Node i t) = "Node: " ++ show i ++ ", " ++ shows t ""
formatPoint (Leaf label) = "Leaf: " ++ label

printTree :: Tree -> Int -> IO ()
printTree Empty _ = return ()
printTree (Tree point left right) indent = do
    putStrLn $ replicate indent ' ' ++ formatPoint point
    printTree left (indent + 2)
    printTree right (indent + 2)

-- ================= MAIN FUNCTION ==================

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-1", treeFile, dataFile] -> do
            catch (do
                treeContent <- readFile treeFile
                let treeLines = map (cleanLine . filter (/= '\r')) (lines treeContent)
                let indexedNodes = zip (zip [0..] (map countSpaces treeLines)) (map splitSpecial (map removeAllSpaces treeLines))
                let (tree, _) = buildTree indexedNodes (head indexedNodes, tail indexedNodes) (-1)

                dataContent <- readFile dataFile
                let newInputs = map parseInput (lines dataContent)
                let results = map (`classify` tree) newInputs

                mapM_ putStrLn results
                )
                errorHandler
        ["-2", trainFile] -> do
            catch (do
                content <- readFile trainFile
                let rows = map parseTrainLine (lines content)
                let tree = train rows
                printTree tree 0
                )
                errorHandler

        _ -> putStrLn "Usage:\n  ./flp-fun -1 <tree_file> <data_file>\n  ./flp-fun -2 <train_file>"
