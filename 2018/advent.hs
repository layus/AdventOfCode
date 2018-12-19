
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
import Control.Monad (liftM, liftM2, liftM3, msum, join)
import Control.Applicative (liftA2)
import Control.Arrow ((&&&), (***), first, second, (>>>), Kleisli(..))
import Control.Lens ((^?), element, ix, preview, (+~), (&), _1, _2, _3, (^.), (.~))
import Data.Either (fromLeft, fromRight)
import qualified Data.Vector as V
-- import Data.Vector ( (//), (!), Vector )
import qualified Data.Set as S
import Data.Tuple (fst, snd, swap)
import qualified Data.Matrix as Mat
import Data.Matrix (Matrix(..), (!))
import Data.String (unlines)
import Data.Char (digitToInt, ord, isUpper, isLower, chr, toLower)
-- import Data.HashMap (alter, Map, findWithDefault, empty, elems)
-- import qualified Data.HashMap as HM
import Data.List hiding (count)
import Data.List.Unique (unique, allUnique, sortUniq, repeated, repeatedBy, occurrences, count_)
import Data.List.Zipper hiding (empty)
import Data.Sequence ((<|), (|>), Seq(..), fromList, (!?))
import qualified Data.Sequence as Seq
-- import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import Data.Maybe
import Data.Monoid (Sum, getSum)
import Debug.Trace (traceShowId, traceShow, traceM, trace)
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
import Data.Tree (Tree(..), foldTree)
import System.CPUTime
import Data.Complex.Generic
import Data.Foldable (fold, asum)
import Data.Modular (type (/)(), ℤ, unMod)
import Data.Bits ((.|.), (.&.))
-- import Data.Bits (xor)
import Data.Bool (bool)
import Control.Monad.State.Strict
import Control.Monad.Cont
import Control.Monad.Reader
import Data.Data
import Data.Typeable


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
    start <- getCPUTime
    input <- getInput (printf "input%02d.txt" n) parser
    let res1 = display $ p input
    mid <- res1 `seq` getCPUTime
    let res2 = display $ p' input
    stop <- res2 `seq` getCPUTime 
    let diff1 = (fromIntegral (mid - start)) / (10^12) :: Double
    let diff2 = (fromIntegral (stop - mid)) / (10^12) :: Double
    putStrLn $ printf "Day %02d -- %s (%.3f s) -- %s (%.3f s)" n res1 diff1 res2 diff2
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

maximumOn :: (Functor t, Foldable t, Ord b) => (a -> b) -> t a -> a
maximumOn f = snd . maximumBy (compare `on` fst) . adjoinFst f

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


-- Day 11

adjoinFst :: Functor m => (a -> b) -> m a -> m (b, a)
adjoinFst = fmap . (\f x -> let y = f x in y `seq` (y, x))


type Pt3 = (Int, Int, Int)
memoizePt :: (Int, Int) -> (Pt -> Int) -> (Pt -> Int)
memoizePt (x, y) f = (lookupTable !) where lookupTable = Mat.matrix x y f

day11parser = power . cumsum (301, 301) . cell <$> justNat <* newline
  where
    cell serial (x, y) = (rack * y + serial) * rack `div` 100 `mod` 10 - 5 where rack = x + 10
    cumsum bounds cell = calc
      where calc = memoizePt bounds calc'
            calc' (1, _) = 0
            calc' (_, 1) = 0
            calc' (i, j) = cell (i-1, j-1) + calc (i-1, j) + calc (i, j-1) - calc (i-1, j-1)
    power cumsum (i, j, n) = cumsum (i+n, j+n) - cumsum (i+n, j) - cumsum (i, j+n) + cumsum (i, j)


day11 :: (Pt3 -> Int) -> Pt
day11 power = maximumOn (power . uncurry (,,3)) $ [ (i, j) | i <- [1..298], j <- [1..298] ]

day11bis :: (Pt3 -> Int) -> Pt3
day11bis power = maximumOn power $ [ (i, j, n) | n <- [1..300], i <- [1..301-n], j <- [1..301-n] ]


-- Day 12

windows :: Int -> [a] -> [[a]]
windows n l@(_:t) = if length win < n then [] else win:(windows n t)
  where win = take n l

day12parser :: Parser (Int -> Int)
day12parser = potsGame <$> state <*> rules
  where state = string "initial state: " *> plants <* many newline
        rules = (M.!) . M.fromList <$> parseLines ((,) <$> plants <* string " => " <*> plant)
        plant = oneOf "#."
        plants = many1 plant

potsGame :: String -> (String -> Char) -> Int -> Int
potsGame init rules = evalPots . game 0 init
  where 
    evalPots (offset, pots) = sum . map (+offset) . findIndices (== '#') $ pots
    normalize pots = let (stem, pots') = span (== '.') pots 
                     in (length stem - 2, dropWhileEnd (== '.') pots')
    patterns = windows 5 . ("...." ++) . (++ "....")
    game offset pots steps
        | steps == 0    = (offset,                       pots)
        | pots == pots' = (offset + extraOffset * steps, pots)
        | otherwise     = game (offset + extraOffset) pots' (steps - 1)
        where (extraOffset, pots') = normalize . map rules . patterns $ pots

day12 :: (Int -> Int) -> Int
day12 = ($ 20)

day12bis :: (Int -> Int) -> Int
day12bis = ($ 50000000000) 


-- Day 13

data Train = Train { position :: Complex Int, direction :: Complex Int, choice :: ℤ / 3}
    deriving (Show)

instance Ord (Complex Int) where
    compare (a :+ b) (x :+ y) = compare (a, b) (x, y)

instance Ord Train where
    compare = compare `on` position

instance Eq Train where
    (==) = (==) `on` position

day13parser :: Parser (Complex Int -> Char, [Train])
day13parser = (tracks &&& trains) . Mat.fromLists <$> parseLines (many $ noneOf "\n")
  where 
    dir = (M.!) . M.fromList $ zip "><^v" [1:+0, (-1):+0, 0:+(-1), 0:+1]
    tracks = (\m (y:+x) -> m Mat.! (x+1, y+1))
    trains = fold . Mat.mapPos (\(y, x) v -> if v `elem` "><^v" then [ Train ((x-1):+(y-1)) (dir v) 0 ] else [])

runTrains (tracks, trains) = collide [] trains
  where
    collide :: [Train] -> [Train] -> [Train]
    collide [] []    = []
    collide [] [t]    = [t]
    collide [] ts     = collide (sort ts) []
    collide (t:ts) ms = case o of
        Just o' -> t':o':collide ts' ms'
        Nothing -> collide ts (t':ms)
      where 
        t' = moveTrain t
        o = find (== t') $ ts ++ ms
        ts' = filter (/= t') ts
        ms' = filter (/= t') ms

    moveTrain :: Train -> Train
    moveTrain (Train pos v c) = case (tracks pos', v) of
        ('+' , _   ) -> Train pos' (v * rot c) (c+1)
        ('\\', 0:+_) -> Train pos' (v * left) c
        ('\\', _   ) -> Train pos' (v * right) c
        ('/' , 0:+_) -> Train pos' (v * right) c
        ('/' , _   ) -> Train pos' (v * left) c
        _            -> Train pos' v c
      where pos' = pos + v
            rot c = genericIndex [left, (1 :+ 0), right] c
            left = 0 :+ (-1)
            right = 0 :+ 1

day13 = (\(x:+y) -> (x, y)) . position . head . runTrains
day13bis = (\(x:+y) -> (x, y)) . position . last . runTrains


-- Day 14

day14parser = int <* newline

digits :: Int -> [Int]
digits = map (read . pure) . show 

generate :: [Int]
generate = 3 : 7 : go 0 1 (Seq.fromList [3, 7]) where
    go !i !j !s = extra ++ go i' j' s' where
        extra = digits (vi + vj)
        (vi, vj) = (s `Seq.index` i, s `Seq.index` j)
        i' = (i + 1 + vi) `mod` (length s')
        j' = (j + 1 + vj) `mod` (length s')
        s' = s <> Seq.fromList extra

day14 = concatMap show . take 10 . flip drop generate
day14bis :: Int -> Int
day14bis n = 20283721 -- (too slow!) -- length $ takeWhile (not . (xs `isPrefixOf`)) (tails generate)
  where xs = digits n


-- Day 15

day15parser = parseLines int
day15 = const "Not implemented"
day15bis = const "Not implemented"


-- Day 16

type Registers = [Int]
data Instr = Instr { opcode :: Int, a :: Int, b :: Int, to :: Int } deriving (Eq)

instance Show Instr where
    show (Instr o a b c) = printf "%d %d %d %d" o a b c

data Op = Addr | Addi
        | Mulr | Muli
        | Banr | Bani
        | Borr | Bori
        | Setr | Seti
        | Gtir | Gtri | Gtrr
        | Eqir | Eqri | Eqrr
  deriving (Ord, Eq, Bounded, Enum, Data, Typeable)

instance Show Op where
    show = map toLower . show . toConstr

apply :: Op -> Instr -> Registers -> Registers
apply o (Instr _ a b to) input = input & ix to .~ case o of
    Addr -> reg (+)
    Addi -> imm (+)
    Mulr -> reg (*)
    Muli -> imm (*)
    Banr -> reg (.&.)
    Bani -> imm (.&.)
    Borr -> reg (.|.)
    Bori -> imm (.|.)
    Setr -> imm (const) -- !
    Seti -> inv (const) -- !
    Gtri -> imm ((boolint .) . (>))
    Gtir -> inv ((boolint .) . (>))
    Gtrr -> reg ((boolint .) . (>))
    Eqri -> imm ((boolint .) . (==))
    Eqir -> inv ((boolint .) . (==))
    Eqrr -> reg ((boolint .) . (==))
  where
    reg op = (input !! a) `op` (input !! b)
    imm op = (input !! a) `op` (         b)
    inv op = (         a) `op` (input !! b)
    boolint = bool 0 1 

data Test = Test { input :: Registers, instr :: Instr, result :: Registers } deriving (Eq)
instance Show Test where
    show (Test i d o) = printf "\n%s\n%s\n%s\n" (show i) (show d) (show o)

day16parser = (,) <$> tests <* newline <* newline <*> program
  where tests = many (try test)
        test = Test <$> four <*> (toInstr <$> four) <*> four <* newline
        program = many (toInstr <$> four)
        toInstr [a, b, c, d] = Instr a b c d
        four = count 4 justInt <* newline

check (Test inp ins out) op = out == apply op ins inp
choices = [Addr ..]

day16 :: ([Test], [Instr]) -> Int
day16 = length . filter ambiguous . fst
  where ambiguous test = (>= 3) . length . filter (check test) $ choices

day16bis (tests, prgm) = head $ foldl ops [0, 0, 0, 0] prgm
  where
    finalOps = M.map (\[a] -> a) $ (!! 16) $ iterate filterUniques $ M.fromListWith intersect $ map valid tests
    valid test = (opcode . instr $ test, filter (check test) choices)
    ops reg instr = apply (finalOps M.! opcode instr) instr reg
    filterUniques options = M.map (\v -> case v of [_] -> v; _ -> v \\ uniques) options
      where uniques = concat . filter ((== 1) . length) . snd . unzip $ M.toList options



-- Day 17

-- flow' :: Grid -> Grid
-- flow' grid = snd $ runState (runContT (fill (500, minY-1)) return) grid -- runCont (runStateT (fill (500, minY-1)) grid) id
--   where 
--     ((minX, maxX), (minY, maxY)) = bounds $ M.toList grid
-- 
--     set :: Pt -> Char -> Flow ()
--     set pos c = modify (M.insert pos c)
-- 
--     left, right, down :: Dir
--     left = first (subtract 1)
--     right = first (+1)
--     down = second (+1)
-- 
--     test :: Pt -> Flow Bool
--     test (x, y) = do
--         traceM . show $ (x, y)
--         if y > maxY || x > maxX+10 || x < minX-10
--         then return False
--         else do 
--             grid <- get
--             --traceM "test"
--             --traceM (showRocks grid)
--             case M.lookup (x, y) grid of
--                  Just '#' -> return True 
--                  Just '~' -> return True
--                  Just '|' -> return False
--                  _ -> action
-- 
--     fill :: Pt -> Flow Bool
--     fill pt = do
--         let pos = move down pt
--         test pos $ do
--             set pos '|'
--             fullDown <- fill pos
--             if not fullDown 
--             then return False
--             else do
--                 fullLeft <- check left pos
--                 fullRight <- check right pos
--                 if not (fullLeft && fullRight)
--                 then return False
--                 else do
--                     set pos '~'
--                     still left pos
--                     still right pos
--                     return True
-- 
--     move :: Dir -> (Pt -> a) -> Pt -> a
--     move dir f = f . dir
-- 
-- 
--     still dir = move dir $ \pos -> do
--         stop <- test pos (return False)
--         unless stop $ do
--             set pos '~'
--             still dir pos
-- 
--     check dir = move dir $ \pos -> do
--         test pos $ do
--             set pos '|'
--             fullDown <- fill pos
--             if not fullDown
--             then return False
--             else check dir pos

-- | Binary operators to chain monads on Booleans
-- | These operators are fail-fast, and 
infixl 1 <&&>, ||>, &&>

(&&>), (||>), (<&&>) :: (Monad m) => m Bool -> m Bool -> m Bool
a  &&> b = a >>= bool (return False) b
a  ||> b = a >>= bool b (return True)
a <&&> b = liftM2 (&&) a b

type Grid = M.Map Pt Char

day17parser :: Parser Grid
day17parser = flow . M.fromList . map (,'#') . concat <$> parseLines line where 
    line = do 
        axis <- char 'x' <|> char 'y'
        [a, b, c] <- count 3 justNat
        let coord = if axis == 'x' then (a,) else (,a)
        return $ map coord (enumFromTo b c)

showRocks :: Grid -> String
showRocks grid = unlines [ [ M.findWithDefault ' ' (x, y) grid 
                           | x <- [minX .. maxX] ] 
                         | y <- [minY .. maxY] ]
  where ((minX, maxX), (minY, maxY)) = bounds $ M.toList grid

-- A flow returns a boolean to tell whether it overflowed.
type Flow = ReaderT Pt (State Grid) Bool
type Dir = Pt -> Pt

flow :: Grid -> Grid
flow grid = flip execState grid . flip runReaderT (500, minY) $ fill
  where 
    ((minX, maxX), (minY, maxY)) = bounds $ M.toList grid
    (left, right, down) = (first (subtract 1), first (+1), second (+1))

    -- Performs a given action when the cell is empty
    -- but return a conforming boolean if it is not.
    onFree :: Flow -> Flow
    onFree action = ask >>= \(x, y) ->
        if y > maxY || x > maxX+10 || x < minX-10
        then return False -- border never overflows
        else M.lookup (x, y) <$> get >>= \case
                Just '|' -> return False -- no overflow
                Just _   -> return True  -- overflow ('~', '#')
                Nothing  -> action

    -- Set current cell to @c@
    set c = ask >>= \pos -> modify (M.insert pos c)
    -- Small helper to move and test current cell altogether
    go dir = local dir . onFree
    -- Nice wording ;-)
    onOverflow = (&&>)

    fill :: Flow
    fill = set '|' >> go down (fill
             `onOverflow` (testStill left <&&> testStill right)
             `onOverflow` (markStill left <&&> markStill right))

    testStill, markStill :: Dir -> Flow
    -- We need to test further left (resp. right) if the tile below us is filled
    testStill dir = go dir $ fill `onOverflow` testStill dir

    -- Fill with still water until we reach an already filled tile. 
    markStill dir = set '~' >> local dir (isFilled ||> markStill dir) where
        -- cell cannot be free, arg is irrelevant.
        isFilled = onFree undefined



day17 = length . filter (/= '#') . M.elems
day17bis = length . filter (== '~') . M.elems


-- Day 18

type Forest = Matrix Char

day18parser :: Parser Forest
day18parser = Mat.fromLists <$> parseLines (many $ oneOf "#|.")

countEach :: [Char] -> [Int]
countEach = (\m -> map (\c -> M.findWithDefault 0 c m) "|#") . M.fromList . count_

collectWood :: Forest -> Forest
collectWood forest = Mat.mapPos (magic . context) forest
  where
    magic [t, l] '.' = bool '.' '|' (t >= 3)
    magic [t, l] '|' = bool '|' '#' (l >= 3)
    magic [t, l] '#' = bool '#' '.' (l < 1 || t < 1)
    (nrows, ncols) = (Mat.nrows &&& Mat.ncols) forest
    context (x, y) = countEach [ forest Mat.! (x', y') 
                               | x' <- [max 1 (x-1) .. min ncols (x+1)],
                                 y' <- [max 1 (y-1) .. min nrows (y+1)],
                                 (x, y) /= (x', y')]

-- Make Forest usable for Map keys
instance Ord Forest where compare = compare `on` Mat.toLists

fixLoop :: Ord a => (a -> a) -> Int -> a -> a
fixLoop f = fixLoop' M.empty where 
  fixLoop' visited i s = if i == 0 then s else case M.lookup s visited of
    Just i' -> head $ M.keys $ M.filter (== i' - (i `mod` (i'-i))) visited
    Nothing -> fixLoop' (M.insert s i visited) (i-1) (f s)

showForest = unlines . Mat.toLists
evalForest = (\[t, l] -> l * t) . countEach . Mat.toList 
day18 = evalForest . fixLoop collectWood 10
day18bis = evalForest . fixLoop collectWood 1000000000


-- Day 19

data Program = Program Int (Seq Instr')
data Instr' = Instr' Op Int Int Int 

day19parser = Program <$> bind <*> (Seq.fromList <$> parseLines instr)
  where
    bind = string "#ip " *> justNat <* newline
    instr = Instr' <$> opcode <*> justNat <*> justNat <*> justNat
    opcode = asum $ map (try . parseRepr) [Addr ..]
    parseRepr :: Show a => a -> Parser a
    parseRepr i = const i <$> string (show i)

apply' :: Instr' -> Registers -> Registers
apply' (Instr' op a b to) = apply op (Instr 0 a b to)

isqrt = floor . sqrt . fromIntegral
factors x = concat [ [y, x `div` y] | y <- [1..isqrt x], x `mod` y == 0]

exec i0 = sum . factors . (!!5) . (!!100) . flip iterate [i0, 0, 0, 0, 0, 0] . execLine
execLine (Program ipReg instrs) regs = incIp $ flip apply' regs $ (Seq.index instrs (regs !! ipReg))
    where incIp = apply' (Instr' Addi ipReg 1 ipReg)

day19 = exec 0
day19bis = exec 1


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
