import List
import System.Random
import Data.Array.IO
import Control.Monad

data Vec = Vec {lst :: [Float]}
	deriving (Show, Eq)
data V = V Int | Float | Double
	deriving (Show, Eq)

class (Num a, Eq a, Show a) => Mag a where
	mag :: a -> Float
	divide :: a -> Float -> a
	zero :: Int -> a

instance Num Vec where
	(+) (Vec one) (Vec two) = 
		Vec $ zipWith (+) one two
	(-) (Vec one) (Vec two) = 
		Vec $ zipWith (-) one two
	(*) (Vec one) (Vec two) =
		Vec $ zipWith (*) one two
	abs (Vec one) = 
		Vec $ map abs one
	
instance Mag Vec where
	--mag :: Vec a -> a
	mag (Vec one) =
		let sqr = map (\x -> x*x) one in
		foldr (+) 0 sqr
	divide (Vec vec) sca =
		Vec $ map (\x -> x / sca) vec
	zero i =
		Vec v where	
			v = map (\x -> 0.0) [1..i]

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

shortest :: (Num b, Ord b) => [[a]] -> b
shortest llst =
	foldr min x xs where
		(x:xs) = map genericLength llst  
	

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

groupNearest :: (Mag a) => [a] -> [a] -> [[a]]
groupNearest points means =
	let dists = map
		(\x -> (x, map (\y -> mag $ x - y) means))
		points in
	let clusters = map
		(\i -> filter 
			(\(_,meanlst) -> leastElem meanlst $ meanlst !! i)
			dists) [0..((genericLength means) - 1)] in
	map (\el -> map (\(v,mlst) -> v) el) clusters

findMean :: (Mag a) => [[a]] -> [a]
findMean points =
	map (\group ->
			let sum = foldr (\x acc -> x + acc) (zero shortlst) group in
			divide sum $ genericLength group)
		points where
	shortlst = shortest points

stop_now :: (Eq a) => [[a]] -> [[a]] -> Bool
stop_now a b = a == b

loop_means :: (Mag a) => [a] -> [[a]] -> Int -> [[a]]
loop_means points old k =
	let means = findMean old in
	let next = groupNearest points means in
	case stop_now old next of
		True -> next
		False -> loop_means points next k 
	

kmeans :: (Mag a) => [a] -> Int -> IO [[a]]
kmeans points k = do
	shuffed <- shuffle points
	let means = take k shuffed
	let begin = groupNearest points means
	return $ loop_means points begin k 	

--wordsToVector :: [String] -> [Vec a]
--wordsToVector wrds =
	
