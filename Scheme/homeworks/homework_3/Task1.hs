module Task1 where 
-- Даниел Здравков Стоянов, 45574, Информатика, 1-ва група
data Tree a = EmptyTree | Node {
                            value :: a,
                            left  :: Tree a,
                            right :: Tree a
                          } deriving (Show,Read)

data Strategy = Inorder | Postorder | Preorder deriving (Show,Read)

inorder :: Tree a -> [a]
inorder EmptyTree = []
inorder (Node root left right) =  inorder left ++ [root] ++ inorder right

preorder :: Tree a -> [a]
preorder EmptyTree = []
preorder (Node root left right) = [root] ++ preorder left ++ preorder right

postorder :: Tree a -> [a]
postorder EmptyTree = []
postorder (Node root left right) = postorder left ++ postorder right ++ [root] 

values :: Strategy -> (Tree a) -> [a]
values s EmptyTree = []
values s t = case s of Inorder -> inorder t 
                       Postorder -> postorder t 
                       Preorder -> preorder t
  
