-- Exercise N4
import Data.Maybe (isJust)
import Data.List (isPrefixOf)

data Tree a = Empty 
             | Node a (Tree a) (Tree a)

pruned :: Tree Int
pruned = Node 5
                Empty
                (Node (-10) 
                    Empty
                    Empty)

testTree = Node 5
                (Node 6 Empty Empty)
                (Node (-10) 
                    (Node 2 Empty Empty)
                    Empty)

bloomed :: Tree Int
bloomed = Node 5
               (Node 6
                     (Node 6 Empty Empty)
                     (Node 6 Empty Empty))
               (Node (-10)
                     (Node 2
                           (Node 2 Empty Empty)
                           (Node 2 Empty Empty))
                     Empty)

{-data Maybe a = Nothing
             | Just a  -}

balanced :: Tree a -> Bool
balanced Empty = True
balanced (Node _ left right) = balanced left && balanced right 
                             && abs (height left - height right) <= 1
  where height = undefined


balanced' :: Tree a -> Bool
balanced' t = fst $ helper t 
  where helper :: Tree a -> (Bool, Int)
        helper Empty = (True, 0)
        helper (Node _ left right)
          | leftBal && rightBal && abs(leftH-rightH) <= 1 = (True, h)
          | otherwise                                     = (False, h)
          where (leftBal, leftH) = helper left
                (rightBal, rightH) = helper right
                h = 1 + max leftH rightH

balanced'' :: Tree a -> Bool
balanced'' t = helper t >= 0 
  where helper :: Tree a -> Int
        helper Empty = 0
        helper (Node _ left right)
          | leftH>=0 && rightH>=0 && abs(leftH-rightH) <= 1 = h
          | otherwise                                      = (-1)
          where leftH = helper left
                rightH = helper right
                h = 1 + max leftH rightH

-- isJust, isNothing

balanced''' :: Tree a -> Bool
--balanced''' t = case helper t of Just _ -> True
--                                 Nothing -> False
balanced''' t = isJust (helper t)
  where -- връща (Just височината) ако е балансирано
        -- връща Nothing иначе
        helper :: Tree a -> Maybe Int
        helper Empty = Just 0
        helper (Node _ left right) = 
            case (helper left, helper right) of (Just lh, Just rh) -> Just (1 + max lh rh)
                                                _ -> Nothing

-- Трябва конструкторите да имат различни имена от другите data
data BST a = BSTEmpty
           | BSTNode a (BST a) (BST a)

-- Task 3:
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

lenght' :: [a] -> Int
lenght' xs = foldr (\x res-> 1 + res) 0 xs

{-
findIndex :: [a] -> Int -> Maybe Int
findIndex [] _ = Nothing 
findIndex xs n = if n >= lenght' xs then Nothing else Just (xs !! n)-}

stripPrefix :: Eq a => [a] -> [a] -> Maybe [a] 
stripPrefix lst1 lst2
  | lst1 `isPrefixOf` lst2 = Just $ drop (lenght' lst1) lst2
  | otherwise              = Nothing

stripPrefix' :: Eq a => [a] -> [a] -> Maybe [a] 
stripPrefix' [] lst2 = Just lst2 
stripPrefix' lst1 [] = Nothing
stripPrefix' (x:xs) (y:ys)
  | x == y  = stripPrefix' xs ys
  | otherwise = Nothing

safeUncons :: [a] -> Maybe (a, [a])
safeUncons [] = Nothing
safeUncons (x:xs) = Just (x, xs)

{-
findIndex :: Eq a => a -> [a] -> Maybe Int 
findIndex _ [] = Nothing 
findIndex 0 (x:_) = x
findIndex n (_:xs) = findIndex (n-1) xs
-}

-- Task 8:
rotateLeft, rotateRight :: Tree a -> Tree a
rotateLeft (Node p a (Node q b c)) = Node q (Node p a b) c 
rotateLeft t = t
rotateRight (Node q (Node p a b) c) = (Node p a (Node q b c)) 
rotateRight t = t


-------------------- GRAPHS --------------------
type Graph = [[Int]]

testGraph :: Graph
testGraph = [[1,2]
            ,[2,5]
            ,[3]
            ,[5]
            ,[]
            ,[4]]

graphSize :: Graph -> Int
graphSize = lenght'

neighbs :: Int -> Graph -> [Int]
neighbs u g = g !! u

-- DFS
-- colors = replicate n White
-- foreach u in g // [0..n-1]
--   if colors [u] == White
--     dfsVisit u g

-- DFSVisit 
-- colors[u] = Gray
-- foreach v in neighbs u
--   if colors[v] == White
--      DFSVisit v g
--  colors[u] = Black

data Color = White | Gray | Black deriving Eq
type State = [Color]

update :: Int a -> [a] -> [a]
update idx val lst = (take idx lst) ++ (val:drop (idx+1) lst)

dfs g = foldl helper (replicate n White) [0 ..n-1]
  where n = graphSize g 
        helper :: State -> Int -> State
        helper colors u 
          | colors !! u == White = dfsVisit u colors
          | otherwise            = colors 
        dfsVisit :: Int -> State -> State 
        dfsVisit u colors = let colors' = foldl helper' (update u Gray colors) (neighbs u g)
                            in update u Black colors' 
          where helper' :: State -> Int -> State 
                helper'  colors v ...5
                  
