-- Exercise N1

-- Task 1:
fib n
  | n < 2     = n
  | otherwise = fib (n-1) + fib (n-2)

-- Task 2:
{-
fib n = if n < 2 then n else fib (n-1) + fib (n-2)

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fib n = (case n of 0 -> 0
                   1 -> 1
                   _ -> fib (n-1) + fib (n-2) )
-}

-- Task 3:
fastPow _ 0 = 1
fastPow x 1 = x
fastPow x n
  | even n    = half*half
  | otherwise = half*half*x
  where half  = fastPow x (n `div` 2)

-- Task 4:
-- complAdd p1 p2 = (fst p1 + fst p2, snd p1 + snd p2)
complAdd (x1,y1) (x2,y2) = (x1+x2, y1+y2)
complSub (x1,y1) (x2,y2) = complAdd (x1,y1) (-x2, -y2)
complMul (x1, y1) (x2, y2) = (x1*x2 - y1*y2, x1*y2 + y1*x2)

 -- Task 5:
distance (x1, y1) (x2, y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

-- Task 6 - lambda fuctions: 
repeated _ 0 = \x -> x
repeated f n = \x -> f ((repeated f (n - 1)) x)

repeatedGuards f n 
  | n == 0    = \x -> x
  | otherwise = \x -> f ((repeated f (n - 1)) x)

-- ---------------------------------------------------------------
-- Side notes:
-- :reload 

-- comments

-- div 15 2 --> 7
-- mod 15 2 --> 1
-- 15 `div` 2 --> 7

-- elem 3 [1,2,3,4]
-- 3 `elem` [1,2,3,4]

-- // equal --> ==
-- // not equal --> /=

-- function init:
-- sample_func x y z = x + y * z

-- if cond
-- (if <cond> then <val1> else <val2>)



-- Guards:
{-fib n
  | n < 2     = n
  | n == 5    = 5
  | otherwise = fib (n-1) + fib (n-2)-}

-- Pattern matching:
{-fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)-}

{-isA 'a' = True
isA 'A' = True
isA _ = False-}

-- foo 0 _ = "iei"
-- foo _ 0 = "bah"
-- foo x y = "w/e"

{-foo x = (let y = fib x in 
            if y > 100 then y else 52)

bar x = if y > 100 then y else 42
  where y = fib x
        z = 2*y + x
        fact 0 = 1
        fact n = n * fact (n-1)-}

-- Lambda:
-- (\ x -> x * x)

{-repeated f n
  | n == 0    = \x -> x
  | otherwise = \x -> f ((repeated f (n-1)) x)-}

  -- succ == increment
  -- pred == decrement

  -- Lists:
  {-
    head [1,2,3,4]
    tail [1,2,3,4]
    null []
    null [1]
    null [[]]
    1 : [2,3,4]
   -}
   
   --length' lst = if null lst then 0 lese 1 + length' (tail lst)

{-
length' [] = 0
length' [_:xs] = 1 + length' xs
-}

-- let nats = [1..]
-- take 4 nats --> [1,2,3,4]

-- indexing !!
-- evens !! 10

-- let odds [1,3,..]
-- let arrToTen [1,3,.10]
-- let arr [20,10,..1]
