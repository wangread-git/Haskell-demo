--Haskell函数名不能以大写开头，以下是错误的
--Haha = 1 + 1

addTwo :: Int -> Int -> Int
addTwo x y = x + y

lucky :: (Integral a) => a -> String
lucky 1 = "One"
lucky 2 = "Two"
lucky 3 = "Three"
lucky 4 = "Four"
lucky 5 = "Five"
lucky x = "Not between 1 and 5"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial(n - 1)

--addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
--addVectors a b = (fst a + fst b, snd a + snd b)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

mySum :: (Num a) => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise                   = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

myMax :: (Ord a) => [a] -> a
myMax [] = error "maximum of empty list"
myMax [x] = x
myMax (x:xs) = max x (myMax xs)

listLess :: (Ord a) => a -> [a] -> [a]
listLess n xs = [x | x <- xs, x <= n]

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
    let smallerSorted = quickSort [a | a <- xs, a <= x]
        biggerSorted = quickSort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

unsafeheader = \(x:_) -> x

mytuple :: [Int] -> String -> ([Int], String)
mytuple a b = (a, b)

--理解currying function的时候，用let代入比较好理解
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
--例如 applyTwice (+3) 10，转化为以下过程
--let plus3 = (+3)
--plus3 10，等同于 10 + 3
--plus3 13, 13是上一步的结果，等同于13 + 3
--又比如 applyTwice (++ " HAHA") "HEY"，转化为以下过程
--let haha = (++ " HAHA")
--haha "HEY"，等同于 "HEY" ++ " HAHA",结果是"HEY HAHA"
--haha "HEY HAHA"，等同于 "HEY HAHA" ++ " HAHA",结果是"HEY HAHA HAHA"
