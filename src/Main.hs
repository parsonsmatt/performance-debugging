{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Data.List
import System.Random

data Node  v d = Node { val :: v, info :: d, prior :: Int } 
    deriving (Eq, Show)
data Treap v d = Leaf | Tree {node :: Node v d, left :: Treap v d, right :: Treap v d}
    deriving Show

feedFold :: [a] -> t -> (a -> t -> (b, t)) -> [b]
feedFold [] _ _ = []
feedFold (x:xs) g f = let (y, g') = f x g in
                        y : feedFold xs g' f

empty :: Treap v d
empty = Leaf

buildNode :: (RandomGen g) => (v, d) -> g -> (Node v d, g)
buildNode (v,d) g = let (p, g') = next g in
                    (Node { val = v, info = d, prior = p}, g')

splitTreap :: (Ord v) => Treap v d -> v -> (Treap v d, Treap v d)
splitTreap Leaf _ = (Leaf, Leaf)
splitTreap (tree @ Tree {node = Node { val = x }, left = l, right = r})   v
    | x < v  = let (lt, rt) = splitTreap r v in
                (   Tree { node = node tree, left = l, right = lt },
                    rt  )
    | v <= x = let (lt, rt) = splitTreap l v in
                (   lt, 
                    Tree { node = node tree, left = rt, right = r}  )

mergeTreap :: (Treap v d, Treap v d) -> Treap v d
mergeTreap (Leaf, t) = t
mergeTreap (t, Leaf) = t
mergeTreap ( lt@Tree { node = Node { prior = pl } }, rt@Tree { node = Node { prior = pr }} )
    | pl >= pr = let mt = mergeTreap (right lt, rt) in
                    Tree { node = node lt, left = left lt, right = mt}
    | pl < pr  = let mt = mergeTreap (lt, left rt) in
                    Tree { node = node rt, left = mt, right = right rt}

insertTreap :: (Ord v) => Treap v d -> Node v d -> Treap v d
insertTreap t (z @ Node {val = v}) = 
    let (lt, rt) = splitTreap t v
        t' = Tree { node = z, left = Leaf, right = Leaf} 
     in mergeTreap (mergeTreap (lt, t'), rt)

inOrder :: Treap v d -> [Node v d]
inOrder Leaf = []
inOrder Tree {node = n, left = lt, right = rt} = inOrder lt ++ [n] ++ inOrder rt

insertMany :: (Ord v) => Treap v d -> [Node v d] -> Treap v d
insertMany = foldl' insertTreap 

heightTreap :: Treap v d -> Int
heightTreap Leaf = 0
heightTreap Tree { node = z, left = lt, right = rt } = 1 + max (heightTreap lt) (heightTreap rt)

pprint :: (Show v, Show d) => Treap v d -> IO ()
pprint t = let 
            nspaces n = concat $ replicate n " "

            tabStop = 4

            showTree :: (Show v, Show d) => Int -> Treap v d -> String
            showTree _ Leaf = ""
            showTree n Tree{node = q, left = lt, right = rt} = 
                nspaces n ++ show q ++ "\n" ++ showTree (n + tabStop) lt ++ showTree (n + tabStop) rt  
            in putStrLn $ showTree 0 t

main = do
    g <- getStdGen
    let nulls = repeat ()
        n = 100000
        rxs = take n $ randomRs (1,100000) g  :: [Int]
        nodeList = feedFold (zip rxs nulls) g buildNode
        treap = insertMany empty nodeList

    print $ heightTreap treap

    print $ map (\Node{val = v} -> v) $ inOrder treap
