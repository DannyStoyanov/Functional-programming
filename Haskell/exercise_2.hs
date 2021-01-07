-- Exercise N2
f lst = [x | x <- lst , even x, x>5]

-- Task 1:
--reverse' lst = foldr (\ el res -> res ++ [el]) [] lst 
reverse' lst = foldl (\ res el -> el : res) [] lst
--                    ^ ламбда еквивалентна на flip (:)

-- Task 2:
descartes lst1 lst2 = [ (x,y) | x <- lst1, y <- lst2]

countDivisors n = length [ d | d <- [1.. n-1], n `mod` d == 0]

-- Task 3:
prime n = (countDivisors n) == 1
primes = [ x | x <- [2..] , countDivisors x == 1]

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
compress lst@(h:_) = (h, numHeads) : compress rest
  where (heads, rest) = span (\x -> x == h) lst 
        numHeads = length heads

--maxRepeated lst = foldr (\ (_,n) res -> max n res) 0 (compress lst)

--maxRepeated lst = maximum (map snd (compress lst))
maxRepeated lst = maximum [ n | (_,n) <- compress lst]