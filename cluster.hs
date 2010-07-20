import List
import System.Random
import Data.Array.IO
import Control.Monad

data Vec a = Vec [a]
	deriving (Show, Eq)

type Mean a = Vec a
type Cluster a = [Vec a]

instance (Num a, Eq a, Show a) => Num (Vec a) where
	(+) (Vec one) (Vec two) = 
		Vec $ zipWith (+) one two
	(-) (Vec one) (Vec two) = 
		Vec $ zipWith (-) one two
	(*) (Vec one) (Vec two) =
		Vec $ zipWith (*) one two
	abs (Vec one) = 
		Vec $ map abs one

mag :: (Num a) => Vec a -> a
mag (Vec one) =
	let sqr = map (\x -> x*x) one in
	foldr (+) 0 sqr

divide :: (Num a,Fractional a) => Vec a -> a -> Vec a
divide (Vec vec) sca =
	Vec $ map (\x -> x / sca) vec  

shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

intToFloat :: Int -> Float
intToFloat n = fromInteger $ toInteger n

leastElem :: (Ord a) => [a] -> a -> Bool
leastElem lst el =
	filter (\x -> x < el) lst == [] 

contains :: (Eq a) => [a] -> a -> Bool
contains [] el = False
contains (x:xs) el =
	case x == el of
		True -> True
		False -> contains xs el

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs) =
	if contains xs x then unique xs else x:(unique xs)

count :: (Eq a) => [a] -> a -> Int
count lst el =
	length $ filter (\x -> x == el) lst

frequency :: (Eq a) => [a] -> a -> Float
frequency lst el = 
	intToFloat (count lst el) / genericLength lst

freqList :: (Eq a) => [a] -> [(a,Float)]
freqList lst =
	let uniq = unique lst in
	map (\f -> (f,frequency lst f)) uniq

groupNearest :: (Ord a,Num a) => [Vec a] -> [Mean a] -> [Cluster a]
groupNearest points means =
	let dists = map
		(\x -> (x, map (\y -> mag $ x - y) means))
		points in
	let clusters = map
		(\i -> filter 
			(\(_,meanlst) -> leastElem meanlst $ meanlst !! i)
			dists) [0..((genericLength means) - 1)] in
	map (\el -> map (\(v,mlst) -> v) el) clusters

findMean :: (Fractional a) => [Cluster a] -> [Mean a]
findMean points =
	map (\group ->
			let sum = foldr (\x acc -> x + acc) (Vec [0.0,0.0,0.0]) group in
			divide sum $ genericLength group)
		points

stop_now :: (Eq a) => [Cluster a] -> [Cluster a] -> Bool
stop_now a b = a == b

loop_means :: (Ord a,Fractional a) => [Vec a] -> [Cluster a] -> Int -> [Cluster a]
loop_means points old k =
	let means = findMean old in
	let next = groupNearest points means in
	case stop_now old next of
		True -> next
		False -> loop_means points next k 
	

kmeans :: (Ord a,Fractional a) => [Vec a] -> Int -> IO [Cluster a]
kmeans points k = do
	shuffed <- shuffle points
	let means = take k shuffed
	let begin = groupNearest points means
	return $ loop_means points begin k 	

