-- ============================================================
--  Project: FLP - Decision Tree Classifier
--  Author: Dinara Garipova
--  Login: xgarip00
--  Year: 2024/25
--  Description: Decision tree definition and pretty-printing.
-- ============================================================

-- Node: either a leaf (with class) or a regular node (with feature index and threshold)
data Point = Leaf String | Node Int Float deriving (Show, Eq)

-- Tree: Node (Point) + left and right subtree (recursively Strom)
data Strom = Strom Point (Maybe Strom) (Maybe Strom) deriving (Show, Eq)