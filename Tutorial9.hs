module Tutorial9 where

-- Sudoku solver
-- Based on Bird, "Thinking Functionally with Haskell"

import Data.List (sort,nub,(\\),transpose,genericLength)
import Data.String (lines,unlines)
import Test.QuickCheck


type Row a    = [a]
type Matrix a = [Row a]
type Digit    = Char

digits :: [Digit]
digits = ['1'..'9']

blank :: Digit -> Bool
blank d = d == ' '

-- 1.
group :: [a] -> [[a]]
group = groupBy 3

groupBy :: Int -> [a] -> [[a]]
groupBy n [] = []
groupBy n str | length str >= n = take n str : groupBy n (drop n str)
              | otherwise = groupBy n str

-- 2.
intersperse :: a -> [a] -> [a]
intersperse a [] = [a]
intersperse a (x:xs) = a : [x] ++ intersperse a xs

-- 3.
showRow :: String -> String
showRow lst = concat (intersperse "|" (group lst))

-- 4.

showGrid :: Matrix Digit -> [String]
showGrid str = concat (intersperse ["---------------"] (group [showRow x | x <- str]))

-- 5.
put :: Matrix Digit -> IO ()
put str = putStrLn (unlines (showGrid str)) 

-- 6.
showMat :: Matrix Digit -> String
showMat xs = concat [ dot x | x<-xs ]
   where 
     dot n = [if b == ' ' || b == ',' then '.' else b | b<-n ]

readMat :: String -> Matrix Digit 
readMat xs = groupBy 9 (concat [dot x | x <- [xs]])
        where 
        dot n = [if b == '.' then ' ' else b | b<-n ]

-- 7.
it = "....345....89...3.3....27892.4..6815....4....8765..4.27523....6.1...79....942...."

choices :: Matrix Digit -> Matrix [Digit]
choices xs = [ dot [[i] |i <- x ] | x <- xs]
        where
        dot ns = [ if n == " " then "123456789" else n | n <- ns]

-- 8.
cp :: [[a]] -> [[a]]
cp []       = [[]]
cp (xs:xss) = [ x:ys | x <- xs, ys <- cp xss ]

prop_cp :: [[a]] -> Bool                                  
prop_cp xss = length (cp xss) == product (map length xss)
-- slow: use `quickCheckWith (stdArgs {maxSize = 5})`

expand :: Matrix [Digit] -> [Matrix Digit]
expand [] = [[]]
expand (xs:xss) = [ x:ys | x <- cp xs, ys <- expand xss ]

-- 9.
prop_expand :: Matrix [Digit] -> Bool
prop_expand m = length (expand m) == product [product [length e | e <- i]| i <- m]

-- 10.
easySols :: Integer
easySols = product [product [fromIntegral(length e) | e <- i]| i <- choices easy]  

-- The answer we get for easySols is 78551672112789411833022577315290546060373041 and to calculate the time to get this answer
-- (from the question, it takes a second to generate trillion solutions) so the answer/trillion is in the power of 10^31
-- The universe is in power of 10^17 seconds old so to calculate easySols, it would take a lot longer than how old the universe is

-- 11, 12, 13.
rows, cols, boxs :: Matrix a -> Matrix a
rows xs = xs 
cols = transpose 
boxs xs = map ungroup(ungroup (map cols(map group (map group xs))))
  where
    ungroup :: Matrix a -> [a]
    ungroup = concat

-- 14.
distinct :: Eq a => [a] -> Bool
distinct xs = length(nub xs) == 9

-- 15.
valid :: Matrix Digit -> Bool
valid g = distinct (rows g) && distinct(cols g) && distinct (boxs g)

-- 16.
simple :: Matrix Digit -> [Matrix Digit] 
simple = filter valid . expand . choices
-- The function simple is taking too long to find all the possible solutions to a suduko puzzle so I don't think it's a viable method.

-- 17.
the :: [Digit] -> Digit
the [d] = d

pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row =[ if length i > 1 then [e | e <- i, e `notElem` chklst ] else i | i <- row]
        where chklst = [the r | r <- row, length r == 1]

-- 18.
pruneBy :: (Matrix [Digit] -> Matrix [Digit]) -> Matrix [Digit] -> Matrix [Digit]
pruneBy f = f.map pruneRow.f

prune :: Matrix [Digit] -> Matrix [Digit]
prune = pruneBy boxs . pruneBy cols . pruneBy rows

-- 19.
many :: Eq a => (a -> a) -> a -> a
many g x = if x == y then x else many g y 
           where y = g x
close    ::    (Eq    a,    Ord    a)    =>    [(a,a)]    ->    [(a,a)] 
close    pairs        =        nub    (sort    (pairs    ++     
                                         [    (x,z)    |    (x,y)    <-    pairs, 
                                                 (y',z)    <-    pairs,
                                                y    ==    y'    ]))
-- 20.
extract :: Matrix [Digit] -> Matrix Digit 
extract mtr | all (all single) mtr = map (map the) mtr

single :: [Digit] -> Bool
single ds = length ds == 1

-- 21.
solve :: Matrix Digit -> Matrix Digit 
solve =  extract . many prune . choices
-- Easy and Medium can be solved using this function


-- ** Optional Material

-- 22.
failed :: Matrix [Digit] -> Bool
failed lst = or [ or [ or [blank i | i <- s] | s <- m] | m <- lst ]

-- 23.
solved :: Matrix [Digit] -> Bool
solved lst = and [ and [ length m1 == 1 | m1 <- m2 ] | m2 <- lst]

-- 24.
shortest :: Matrix [Digit] -> Int
shortest xss = length (head (head [snd x | x <- [break (\ds -> length ds == 2) m |m <-xss]]))

-- x = tuple
-- 25.
expand1 :: Matrix [Digit] -> [Matrix [Digit]]
expand1 mat = [preMat ++ [preRow ++ [[d]] ++ postRow] ++ postMat | d <- ds] 
              where 
                      (preMat, row:postMat) = break (any(not . p)) mat 
                      (preRow, ds:postRow) = break (not . p) row 
                      p [_] = True
                      p _ = False

-- 26.
search :: Matrix Digit -> [Matrix Digit]
search = search2 . many prune . choices 
        where
                search2 :: Matrix [Digit] -> [Matrix Digit]
                search2 x | failed x = []
                          | solved x = [extract x]
                          | otherwise = concat (map (search2 . many prune) (expand1 x))
       


-- Example from Bird

book :: Matrix Digit
book = ["  4  57  ",
        "     94  ",
        "36      8",
        "72  6    ",
        "   4 2   ",
        "    8  93",
        "4      56",
        "  53     ",
        "  61  9  "]

-- Examples from websudoku.com

easy :: Matrix Digit
easy = ["    345  ",
        "  89   3 ",
        "3    2789",
        "2 4  6815",
        "    4    ",
        "8765  4 2",
        "7523    6",
        " 1   79  ",
        "  942    "]

medium :: Matrix Digit
medium = ["   4 6 9 ",
          "     3  5",
          "45     86",
          "6 2 74  1",
          "    9    ",
          "9  56 7 8",
          "71     64",
          "3  6     ",
          " 6 9 2   "]

hard :: Matrix Digit
hard = ["9 3  42  ",
        "4 65     ",
        "  28     ",
        "     5  4",
        " 67 4 92 ",
        "1  9     ",
        "     87  ",
        "     94 3",
        "  83  6 1"]

evil :: Matrix Digit
evil = ["  9      ",
        "384   5  ",
        "    4 3  ",
        "   1  27 ",
        "2  3 4  5",
        " 48  6   ",
        "  6 1    ",
        "  7   629",
        "     5   "]

puts :: [Matrix Digit] -> IO ()
puts = sequence_ . map put

puzzle :: Matrix Digit -> IO ()
puzzle g = put g >> puts (search g) >> putStrLn "***"
       
main :: IO ()
main = puzzle easy >>
       puzzle medium >>
       puzzle hard >>
       puzzle evil

