-- Exercise N2

-- Task 0:

myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

myFilter _ [] = []
myFilter p (x:xs)
  | p x       = x : myFilter p xs
  | otherwise = myFilter p xs

myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

myLength [x] = 1
myLength (_:xs) = 1 + myLength xs

myNull [] = True
myNull [_] = False

myElem [] _ = False
myElem (x:xs) y 
  | x == y = True
  | otherwise = myElem xs y

myTake _ [] = []
myTake n (x:xs) 
  | n == 0    = []
  | otherwise = x : myTake (n-1) xs

myDrop _ [] = []
myDrop n lst@(x:xs) 
  | n == 0    = lst
  | otherwise = myDrop (n-1) xs

myZip (x:xs) (y:ys) = (x,y) : myZip xs ys 
myZip _ _ = []

myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys
myZipWith f _ _ = []

myTakeWhile p (x:xs) 
  | p x       = x : myTakeWhile p xs
  | otherwise = []

myDropWhile p lst@(x:xs)
  | p x       = myDropWhile p xs
  | otherwise = lst
  
-- Task 1- rewrite following functions with foldr/foldl
minimum' lst = foldr (\x y -> if x <= y then x else y) (head lst) lst

maximum' lst = foldr (\x y -> if x > y then x else y) (head lst) lst

reverse' lst = foldl (flip (:)) [] lst 
--reverse' lst = foldr (\ el res -> res ++ [el]) [] lst 
--reverse' lst = foldl (\ res el -> el : res) [] lst
--                    ^ ламбда еквивалентна на flip (:)

length' lst = foldr (\_ res -> res + 1) 0 lst

all' p lst = foldr (\x y -> if p x then True && y else False) True lst
{-myAll _ [] = True
myAll p (x:xs) 
  | p x       = myAll p xs
  | otherwise = False-}

any' p lst = foldr (\x y -> if p x then True || y else False || y) False lst 
{-myAny _ [] = False
myAny p (x:xs)
  | p x       = True
  | otherwise = myAny p xs -}

append' lst1 lst2 = foldr (\ x y -> x : y) [] (lst1 ++ lst2)

replicate' n x = foldr (\_ y -> x : y) [] [1..n]
{-myReplicate n x 
  | n == 0    = []
  | otherwise = x : myReplicate (n-1) x-}

-- Task 2:
divisors n = [ d | d <- [1..n], n `mod` d == 0]
countDivisors n = length' (divisors n)

--prime n = if countDivisors n == 2 then True else False

descartes lst1 lst2 = [ (x,y) | x <- lst1, y <- lst2]

-- Task 3:
-- primes = [ x | x <- [2..], prime x == True]
prime n = (countDivisors n) == 2
primes = [ x | x <- [2..] , countDivisors x == 2]

-- Task 4:
sieve (x:xs) = x : sieve (filter (\y -> y `mod` x /= 0) xs)
primesSieve = sieve[2..]
  where sieve lst@(x:xs) = x : sieve [ y | y <- xs, y `mod` x /= 0]

-- Task 5:
naturalPair = [ (d-i, i) | d <- [0..], i <- [0..d] ]

-- Task 6:
pitagorasTuples = [ (x,y,z) | border <- [2..], x<-[1..border], y<-[1..border], z<-[1..border], x*x + y*y == z*z ]

-- Task 7:
compress [] = []
compress lst@(head:tail) = (head, times) : compress next
  where (same, next) = span (\x -> x == head) lst
        times = length' same

-- Task 8:
maxRepeated lst = maximum [ times | (_,times) <- [ pair | pair <- compress lst]]
--maxRepeated lst = foldr (\ (_,n) res -> max n res) 0 (compress lst)
--maxRepeated lst = maximum (map snd (compress lst))
--maxRepeated lst = maximum [ n | (_,n) <- compress lst]

-- Task 9:
makeSet lst = foldr (\ el res -> if el `elem` res then res else el:res) [] lst

-- Task 10:
countAppearences el lst = foldr (\x rest -> if el == x then 1 + rest else 0 + rest) 0 lst

histogram lst@(x:xs) = zip uniques times
  where uniques = makeSet lst
        times = map (\x -> countAppearences x lst ) uniques
{-histogram lst = [ (el,count el) | el<-makeSet lst ]
  where count el = length [ x | x<-lst, x==el ]-}

-- Task 11:
maxDistance pts = maximum [ distance p1 p2 | p1<-pts, p2<-pts ]
  where distance (x1,y1) (x2,y2) = sqrt ((x2-x1)^2 + (y2-y1)^2) 