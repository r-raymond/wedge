module Forms
    where

import Data.List

data BasicForm
    = BasicForm
    { coeff :: Float
    , coords :: [Int]
    }

instance Show BasicForm where
    show (BasicForm a b) = (show a) ++ "e^{" ++ cor ++ "}"
      where
        cor = foldl (++) "" (fmap show b)

hasDupes :: [Int] -> Bool
hasDupes [] = False
hasDupes (x:xs) =
    if x `elem` xs
        then True
        else hasDupes xs

type Form = [BasicForm]

render :: Form -> String
render l = foldl (++) "" l2
  where
    l1 = fmap show l
    l2 = intersperse " + " l1

parity :: (Num a) => [Int] -> a
parity k =
    if even $ inversions k
        then 1
        else -1
  where
    inversions :: [Int] -> Int
    inversions [] = 0
    inversions (x:xs) = length (filter (< x) xs) + inversions xs

wedgeB :: BasicForm -> BasicForm -> BasicForm
wedgeB (BasicForm c1 l1) (BasicForm c2 l2) =
    BasicForm (c1 * c2) (l1 ++ l2)

wedge :: Form -> Form -> Form
wedge f1 f2 = fmap f pro
  where
    pro = [(x,y) | x <- f1, y <- f2]
    f = \(x,y) -> wedgeB x y

filterZeroes :: Form -> Form
filterZeroes f = filter ff f
  where
    ff = \(BasicForm x y) -> (x /= 0) && (not $ hasDupes y) && (length y > 0)

sortCoords :: BasicForm -> BasicForm
sortCoords (BasicForm a b) = BasicForm ((parity b) * a) (sort b)

add :: Form -> Form -> Form
add f1 f2 = prettify (f1 ++ f2)

prettify :: Form -> Form
prettify = factor . (fmap sortCoords) . filterZeroes

factor :: Form -> Form
factor [] = []
factor (x:xs) = (BasicForm newA b) : (factor n)
  where
    a = coeff x
    b = coords x
    r = filter (\(BasicForm _ y) -> b == y) xs
    n = filter (\(BasicForm _ y) -> b /= y) xs
    coeffs = fmap coeff r
    newA = a + (sum coeffs)

phi :: Form
phi =
    [ BasicForm 1 [1,2,3]
    , BasicForm 1 [1,4,5]
    , BasicForm 1 [1,6,7]
    , BasicForm 1 [2,4,6]
    , BasicForm (-1) [2,5,7]
    , BasicForm (-1) [3,4,7]
    , BasicForm (-1) [3,5,6]
    ]

phi2 :: Form
phi2 =
    [ BasicForm (-1) [1,2,3]
    , BasicForm 1 [1,4,5]
    , BasicForm 1 [1,6,7]
    , BasicForm 1 [2,4,6]
    , BasicForm (-1) [2,5,7]
    , BasicForm 1 [3,4,7]
    , BasicForm 1 [3,5,6]
    ]


phi3 :: Form
phi3 =
    [ BasicForm 1 [1,2,3]
    , BasicForm 1 [1,4,5]
    , BasicForm 1 [1,6,7]
    , BasicForm (-1) [2,4,6]
    , BasicForm 1 [2,5,7]
    , BasicForm 1 [3,4,7]
    , BasicForm 1 [3,5,6]
    ]

omega1 :: Form
omega1 =
    [ BasicForm 1 [5, 6]
    , BasicForm 1 [7, 8]
    ]

omega2 :: Form
omega2 =
    [ BasicForm 1 [5, 7]
    , BasicForm (-1) [6, 8]
    ]

omega3 :: Form
omega3 =
    [ BasicForm 1 [5, 8]
    , BasicForm 1 [6, 7]
    ]

psi :: Form
psi = hodge 7 psi

hodgeB :: Int -> BasicForm -> BasicForm
hodgeB n (BasicForm a b) = BasicForm (a * p) diff
  where
    all = [1..n]
    diff = all \\ b
    p = parity (b ++ diff)

hodge :: Int -> Form -> Form
hodge n f = fmap (hodgeB n) f


beta :: Form -> Form
beta x = prettify $ hodge 7 $ prettify (x `wedge` psi)

beta2 :: Form -> Form
beta2 x = prettify $ hodge 7 $ prettify (x `wedge` (hodge 7 phi2))


beta3 :: Form -> Form
beta3 x = prettify $ hodge 7 $ prettify (x `wedge` (hodge 7 phi3))

e :: Int -> Form
e n = [BasicForm 1 [n]]
