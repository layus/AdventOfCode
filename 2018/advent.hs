
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad (liftM2, liftM3, msum, join)
import Control.Applicative (liftA2)
import Control.Arrow ((&&&), (***), first, second, (>>>), Kleisli(..))
import Control.Lens ((^?), element, ix, preview, (+~), (&), _1, _2, _3, (^.))
import Data.Either (fromLeft, fromRight)
import qualified Data.Vector as V
-- import Data.Vector ( (//), (!), Vector )
import qualified Data.Set as S
import Data.Tuple (fst, snd, swap)
-- import Data.Bits (xor)
import qualified Data.Matrix as Mat
import Data.Matrix (Matrix(..), (!))
import Data.String (unlines)
import Data.Char (digitToInt, ord, isUpper, isLower, chr)
-- import Data.HashMap (alter, Map, findWithDefault, empty, elems)
-- import qualified Data.HashMap as HM
import Data.List hiding (count)
import Data.List.Unique (unique, allUnique, sortUniq, repeated, repeatedBy, occurrences, count_)
import Data.List.Zipper hiding (empty)
import Data.Sequence ((<|), (|>), Seq(..), fromList)
import qualified Data.Sequence as Seq
-- import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import Data.Maybe
import Data.Monoid (Sum, getSum)
import Debug.Trace (traceShowId, traceShow)
import Text.Parsec ( many, many1, sepBy, sepBy1, count, (<|>) -- repeats
                   , char, string, noneOf, oneOf, lower, upper, letter -- chars 
                   , try, newline, eof, parse, anyToken, optional)
import Text.Parsec.String (Parser, parseFromFile)
import qualified Text.Parsec.Error
import Text.Printf (printf)
import Text.Parsec.Number (int, nat)
import Data.Function (on)
import Control.Exception (catch)
import System.IO.Error (isDoesNotExistError)
import Data.Tree

-- infixl 7 % -- modulus has same precedence as (*) or (/)
-- (%) = mod


-- Parsers

getInput :: FilePath -> Parser a -> IO a
getInput path p = do
    input <- parseFromFile (p <* eof) path 
    either (error . show) return input

-- getInput2 :: FilePath -> Parser a -> Parser b -> IO (a, b)
-- getInput2 path p1 p2 = do
--     liftM2 (,) (getInput path p1) (getInput path p2)

-- tabMatrix :: Parser [[Int]]
-- tabMatrix = many1 (sepBy1 int (char '\t') <* newline)

parseLines :: Parser a -> Parser [a]
parseLines p = many1 (p <* newline)

-- Read

-- readInputFile :: Int -> IO String
-- readInputFile = readFile . printf "input%02d.txt"
-- 
-- readLine :: Int -> IO String
-- readLine i = head . lines <$> readInputFile i

class Display a where
    display :: a -> String

instance {-# OVERLAPS #-} Display String where
    display = id

instance Show a => Display a where
    display = show

day :: (Display b, Display c) => Int -> Parser a -> (a -> b) -> (a -> c) -> IO ()
day n parser p p' = flip catch ignore $ do
    input <- getInput (printf "input%02d.txt" n) parser
    putStrLn $ printf "Day %02d -- %s -- %s" n (display $ p input) (display $ p' input)
  where ignore e | isDoesNotExistError e = return () 
                 | otherwise = putStrLn $ show e

main = do
    putStrLn "Start ..."
    day 25 day25parser day25 day25bis
    day 24 day24parser day24 day24bis
    day 23 day23parser day23 day23bis
    day 22 day22parser day22 day22bis
    day 21 day21parser day21 day21bis
    day 20 day20parser day20 day20bis
    day 19 day19parser day19 day19bis
    day 18 day18parser day18 day18bis
    day 17 day17parser day17 day17bis
    day 16 day16parser day16 day16bis
    day 15 day15parser day15 day15bis
    day 14 day14parser day14 day14bis
    day 13 day13parser day13 day13bis
    day 12 day12parser day12 day12bis
    day 11 day11parser day11 day11bis
    day 10 day10parser day10 day10bis
    day 9  day09parser day09 day09bis
    day 8  day08parser day08 day08bis
    day 7  day07parser day07 day07bis
    day 6  day06parser day06 day06bis
    day 5  day05parser day05 day05bis
    day 4  day04parser day04 day04bis
    day 3  day03parser day03 day03bis
    day 2  day02parser day02 day02bis
    day 1  day01parser day01 day01bis

hidden = const "xxx"

-- Day 01

day01parser :: Parser [Int]
day01parser = parseLines int

fromBool :: Bool -> a -> Maybe a
fromBool True  = Just
fromBool False = const Nothing

firstDup :: Ord a => [a] -> a
firstDup = fromJust . msum . snd . mapAccumL seen S.empty
  where seen set x = (x `S.insert` set, fromBool (x `S.member` set) x)

day01 :: [Int] -> Int
day01 = sum

day01bis :: [Int] -> Int
day01bis = firstDup . scanl (+) 0 . cycle


-- Day 02

day02parser :: Parser [String]
day02parser = parseLines (many1 lower)

day02 :: [String] -> Int
day02 = uncurry (*) . (has 3 &&& has 2)
  where has x = sum . map (min 1 . length . repeatedBy (== x))

day02bis :: [String] -> String
day02bis = snd . firstDup . concatMap (zip [0..] . delOne [])
  where delOne :: String -> String -> [String]
        delOne prefix (x:xs) = (prefix ++ xs) : delOne (prefix ++ [x]) xs
        delOne prefix [] = []


-- Day 03

untilNat :: Parser Int
untilNat = try $ many (noneOf "0123456789\n") *> nat

day03parser :: Parser [[Int]]
day03parser = parseLines (many1 untilNat)

day03 :: [[Int]] -> Int
day03 = length . repeated . concatMap positions
  where positions [_, x, y, w, h] = [(px, py) | px <- [x..x+w-1], py <- [y..y+h-1]]

day03bis :: [[Int]] -> Int
day03bis xs = head $ head $ filter (\x -> not (any (conflict x) xs)) xs
  -- Not fast enough:
  --    conflict x y = if x == y then False else day03 [x, y] > 0
  where conflict a@[_, x, y, w, h] b@[_, x', y', w', h']
          | a == b = False
          | otherwise = x < x'+w' && x' < x+w && y < y'+h' && y' < y+h


-- Day 04

groupFstWith :: Ord a => ([b] -> c) -> [(a, b)] -> [(a, c)]
-- groupFstWith fold = map (head *** fold) . map unzip . groupBy ((==) `on` fst) . sortBy (compare `on` fst)
groupFstWith fold = map (second fold) . M.toAscList . M.fromListWith (++) . map (second (:[]))

type Naps = [(Int, Int)]
type Shifts = [(Int, Naps)]
day04parser :: Parser Shifts
day04parser = filter (not . null . snd) . groupFstWith concat <$> many shift
  where shift = (,) <$> guard <*> many ((,) <$> try start <*> stop)
        guard = lastNatThen " begins shift"
        start = lastNatThen "] falls asleep"
        stop  = lastNatThen "] wakes up"
        lastNatThen s = last <$> many1 untilNat <* string s <* newline

maximumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> a
maximumOn = maximumBy . (compare `on`)

mostCommon :: Ord a => [a] -> a
mostCommon = fst . last . count_

day04 :: Shifts -> Int
day04 = uncurry (*) . (id *** mostCommon . minutes) . maximumOn lazyness
  where
    minutes = concatMap (\(x, y) -> [x..y-1])
    lazyness = sum . map (uncurry subtract) . snd

day04bis :: Shifts -> Int
day04bis = uncurry (*) . (id *** snd) . maximumOn snd . map (id *** countSleeps)
  where
    minutes = concatMap (\(m, m') -> [m..m'-1])
    -- Naps -> (maxCount, mostFreqMinute)
    countSleeps = swap . head . count_ . minutes


-- Day 05

day05parser :: Parser [Int]
day05parser = many (toInt <$> letter) <* newline
  where toInt l | isUpper l = (ord 'A' - (ord l + 1))
                | isLower l = ((ord l + 1) - ord 'a')

day05 = length . foldr reduce []
  where reduce x (y:ys) = if x+y == 0 then ys else x:y:ys
        reduce x [] = [x]

day05bis = minimum . map day05 . zipWith removeUnit [1..26] . repeat
  where removeUnit i = filter (not . (== i) . abs)


-- Day 06

minimumsBy :: (a -> a -> Ordering) -> [a] -> [a]
minimumsBy compare = reverse . foldl mins []
  where mins [] x = [x]
        mins acc@(a:_) x = case compare a x of
                                GT -> [x]
                                EQ -> (x:acc)
                                LT -> acc

minimumsOn :: Ord b => (a -> b) -> [a] -> [a]
minimumsOn = minimumsBy . (compare `on`)

type Pt = (Int, Int) 
manhathan :: Pt -> Pt -> Int
manhathan (a, b) (c, d) = abs (c-a) + abs (d-b)

day06parser :: Parser [Pt]
day06parser = parseLines ((,) <$> int <* string ", " <*> int)

day06 :: [Pt] -> Int
day06 positions = maximum . map (length . snd) . noInfinite . groupFstWith id . adjoinClosestId $ grid
  where
    grid = [(x, y) | x <- [-150..450], y <- [-150..450], 300 >= manhathan (150, 150) (x, y)]
    border pt = manhathan (150, 150) pt == 300
    noInfinite = filter (not . any border . snd)
    adjoinClosestId = mapMaybe (\x -> (,x) <$> closest x)
    closest pt = case minimumsOn (manhathan pt . snd) . zip [1..] $ positions of
                                [(id, _)] -> Just id
                                _ -> Nothing

day06bis :: [Pt] -> Int
day06bis pos = length . filter (< 10000) . map (sum . flip map pos . manhathan) $ grid
    where grid = [(x, y) | x <- [-150..450], y <- [-150..450], 300 >= manhathan (150, 150) (x, y)]


-- Day 07

untilUpper :: Parser Char
untilUpper = many (noneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ") *> upper

{- Deplist helpers -}
type DepList = [(Char, [Char])]
toDependencyList :: [[Char]] -> DepList
toDependencyList = groupFstWith concat . concatMap (\[a, b] -> [(b, [a]), (a, [])]) 
removeTask :: (Char, a) -> DepList -> DepList
removeTask (x, _) = filter ((/= x) . fst)
finishTask :: (Char, a) -> DepList -> DepList
finishTask (x, _) = map (second $ filter (/= x))

day07parser :: Parser DepList
day07parser = toDependencyList <$> parseLines line
    where line = char 'S' *> count 2 untilUpper <* many (noneOf "\n")

day07 :: DepList -> String
day07 = unfoldr getFirst
  where
    remove x = finishTask x . removeTask x
    getFirst items = (\task -> (fst task, remove task items)) <$> find (null . snd) items

day07bis :: DepList -> Int
day07bis = sum . unfoldr tasks . ([('0',0)], )
  where
    duration = (+61) . subtract (ord 'A') . ord
    toWorker (t, _) = (t, duration t)
    stopWhen = fromBool . not
    tasks (oldWorkers, oldTasks) = stopWhen (null oldWorkers) (jump, (newWorkers, newTasks))
      where
        -- Update workers
        jump = minimum $ map snd oldWorkers                         -- compute largest possible progress
        workers = map (second (subtract jump)) oldWorkers           -- Make progress for each worker
        (finished, remaining) = partition ((== 0) . snd) workers    -- Collect finished tasks
        -- Update tasks
        todoTasks = foldr finishTask oldTasks finished              -- Remove finished tasks from todo list.
        runnableT = filter (null . snd) todoTasks                   -- List available tasks
        -- start as many jobs as possible
        newWorkers = take 5 $ (remaining ++ map toWorker runnableT) -- run max 5 actions from available ones
        newTasks = foldr removeTask todoTasks newWorkers            -- Remove active tasks from todo list


-- Day 08

day08parser :: Parser (Tree [Int])
day08parser = do
    [nChild, nData] <- count 2 untilNat
    flip Node <$> count nChild day08parser <*> count nData untilNat

day08 = foldr ((+) . sum) 0
day08bis = foldTree getValue where
    getValue v [] = sum v
    getValue idx children = sum $ mapMaybe (\i -> children ^? ix (i-1)) idx

{- 
-- | A copy of Guillaume's algorithm
day08parser :: Parser (Int, Int)
day08parser = do
    [nb_children, nb_metadata] <- count 2 untilNat
    (childs, metadata) <- (,) <$> count nb_children day08parser <*> count nb_metadata untilNat
    return $ (sum metadata + (sum $ map fst childs), sum $ if null childs then metadata else [snd $ childs !! (i-1) | i <- metadata, 0 < i && i <= nb_children])
day08 = fst
day08bis = snd
-}

-- Day 09

justNat = untilNat <* many (noneOf "0123456789\n")

day09parser = (,) <$> justNat <*> justNat

shift :: Seq a -> Seq a
shift (xs :|> x) = x <| xs

unshift :: Seq a -> Seq a
unshift (x :<| xs) = xs |> x

game :: (Int, Int) -> (IM.IntMap Int, Seq Int, Int)
game (players, steps) = iterate' (rules players) init !! steps
  where init = (IM.empty, Seq.fromList [0], 1)
rules :: Int -> (IM.IntMap Int, Seq Int, Int) -> (IM.IntMap Int, Seq Int, Int)
rules players (!score, !circle, !marble)
    | marble `mod` 23 == 0 = let x :<| xs = iterate' unshift circle !! 7 in
            (IM.insertWith (+) (marble `mod` players) (marble + x) score, shift xs, marble + 1) 
    | otherwise = (score, marble <| (shift circle), marble + 1)

day09 = maximum . (^. _1) . game
day09bis = day09 . (second (* 100))


-- Day 10

-- aligned (((x, y), _):ps) = (length $ filter (\((a, b), _) -> a == x || b == y) ps) > 40
justInt = skip *> int <* skip 
  where skip = many (noneOf "+-0123456789\n")

day10parser = parseLines $ format <$> count 4 justInt
  where format [x, y, dx, dy] = ((x + 10000 * dx, y + 10000 * dy), (dx, dy))

move ((x, y), (dx, dy)) = ((x + dx, y + dy), (dx, dy))
aligned' = (== 9) . uncurry subtract . snd . bounds -- Merci Guillaume ;-)
bounds = join (***) (minimum &&& maximum) . unzip . map fst 

showStars ps = unlines $ "":[ [ if (x, y) `elem` pos then '▓' else '░'
                              | x <- [minX..maxX] ]
                            | y <- [minY..maxY] ]
  where pos = S.fromList (map fst ps)
        ((minX, maxX), (minY, maxY)) = bounds ps

day10 = showStars . until aligned' (map move)
day10bis = (10000 +) . length . takeWhile (not . aligned') . iterate (map move)


-- Day 10, direct computation

{-
minimumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> a
minimumOn = minimumBy . (compare `on`)

calc ( ((a, b), (da, db)): ((x, y), (dx, dy)): _) 
    | db /= dy = Just $ (liftA2 min id swap) ((y - b - 10) `div` (dy - db), (b - y - 10) `div` (db - dy))
    | otherwise = Nothing
calc _ = Nothing
span = show . foldl' (\(x, y) (a, b) -> (max x a, min y b)) (-50000, 50000) . mapMaybe calc . tails
-- #> (-241, -240) => (-10241, -10240)
timelapse ps = let fast = minimumOn (snd . snd) ps
                   slow = maximumOn (fst . snd) ps
               in calc [fast, slow] 
-}


-- Day 11

cell serial (x, y) = (rack * y + serial) * rack `div` 100 `mod` 10 - 5 where rack = x + 10
day11parser = (\serial -> Mat.matrix 300 300 (cell serial)) <$> justNat <* newline

day11 :: Matrix Int -> (Int, Int)
day11 = snd . maximum . fuel
  where fuel mat = [ (power pos mat, pos) | pos <- [ (i, j) | i <- [1..298], j <- [1..298] ] ]
        power (i, j) = sum . Mat.toList . Mat.submatrix i (i+2) j (j+2)


day11bis :: Matrix Int -> (Int, Int, Int)
day11bis mat = snd $ maximum fuel
  where fuel = [ (s, (i, j, n)) | i <- [1..300], n <- [1..301-i], let (s, j) = maxPow i n ]
        pMat = Mat.fromLists $ scanl' (zipWith (+)) (replicate 300 0) $ Mat.toLists mat
        -- inefficient, as the sum gets calculated every time
        -- power n (i, j) = sum [ pMat ! (i+n, l) - pMat ! (i, l) | l <- [j..j+n-1] ]
        -- nsums :: [Int] -> [Int] -- compute sum of sublists of size n, O(length x)
        nsums n x = drop n $ scanl' (+) 0 $ zipWith subtract (replicate n 0 ++ x) x
        maxPow i n = maximum $ zip (nsums n difflist) [1..]
          where difflist :: [Int]
                difflist = zipWith subtract (V.toList $ Mat.getRow i pMat) (V.toList $ Mat.getRow (i+n) pMat)

{-
day11bis mat = let
    rows = Mat.toLists mat
    prows = scanl' (zipWith (+)) (replicate 300 0) rows
    iprows = zip prows [1..]
    rowpairs = [ (zipWith subtract r r', i, (i' - i)) | ((r, i):t) <- tails iprows, (r', i') <- t ]
    nsums n x = drop n $ scanl' (+) 0 $ zipWith subtract (replicate n 0 ++ x) x
    maxPow (r, (i, n)) = zip (nsums n r) [1..]
    ((_, j), (i, j)) = partialBest = maximum . map (maximum . map bestj) rowpairs 
    in (i, j, n)
-}


    


-- Day 12

day12parser = parseLines int
day12 = const "Not implemented"
day12bis = const "Not implemented"


-- Day 13

day13parser = parseLines int
day13 = const "Not implemented"
day13bis = const "Not implemented"


-- Day 14

day14parser = parseLines int
day14 = const "Not implemented"
day14bis = const "Not implemented"


-- Day 15

day15parser = parseLines int
day15 = const "Not implemented"
day15bis = const "Not implemented"


-- Day 16

day16parser = parseLines int
day16 = const "Not implemented"
day16bis = const "Not implemented"


-- Day 17

day17parser = parseLines int
day17 = const "Not implemented"
day17bis = const "Not implemented"


-- Day 18

day18parser = parseLines int
day18 = const "Not implemented"
day18bis = const "Not implemented"


-- Day 19

day19parser = parseLines int
day19 = const "Not implemented"
day19bis = const "Not implemented"


-- Day 20

day20parser = parseLines int
day20 = const "Not implemented"
day20bis = const "Not implemented"


-- Day 21

day21parser = parseLines int
day21 = const "Not implemented"
day21bis = const "Not implemented"


-- Day 22

day22parser = parseLines int
day22 = const "Not implemented"
day22bis = const "Not implemented"


-- Day 23

day23parser = parseLines int
day23 = const "Not implemented"
day23bis = const "Not implemented"


-- Day 24

day24parser = parseLines int
day24 = const "Not implemented"
day24bis = const "Not implemented"


-- Day 25

day25parser = parseLines int
day25 = const "Not implemented"
day25bis = const "Not implemented"
