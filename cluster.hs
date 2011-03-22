module Cluster (runOnFile) where
import List
import System.Random
import System.IO
import System ( getArgs )
import Data.Array.IO
import Control.Monad
import Control.Exception
import qualified Data.Map as M
import StrictRead

data FloatVec = FloatVec {lst :: [Float]}
	deriving (Show, Eq) 
data WordVec = WordVec {m :: (M.Map String Float), name :: String} 
	deriving (Eq)

class (Num a, Eq a, Show a) => Vec a where
	mag :: a -> Float
	divide :: a -> Float -> a
	zero :: Int -> a

instance Show WordVec where
	show (WordVec a b) = 
		show b

instance Num FloatVec where
	(+) (FloatVec one) (FloatVec two) = 
		FloatVec $ zipWith (+) one two
	(-) (FloatVec one) (FloatVec two) = 
		FloatVec $ zipWith (-) one two
	(*) (FloatVec one) (FloatVec two) =
		FloatVec $ zipWith (*) one two
	abs (FloatVec one) = 
		FloatVec $ map abs one
	signum (FloatVec one) =
		FloatVec $ [foldr (*) 1 signs] where
			signs = map signum one
	fromInteger i = FloatVec [intToFloat $ fromInteger i]

instance Num WordVec where
	(+) (WordVec a s1) (WordVec b s2) =
		WordVec (M.unionWith (+) a b) "+"
	(-) (WordVec a s1) (WordVec b s2) =
		WordVec (M.unionWith (-) a b) "-"
	(*) (WordVec a s1) (WordVec b s2) =
		WordVec (M.unionWith (*) a b) "*"
	abs (WordVec a s1) =
		WordVec (M.map (\v-> abs v) a) s1
	
instance Vec FloatVec where
	--mag :: Vec a -> a
	mag (FloatVec one) =
		let sqr = map (\x -> x*x) one in
		foldr (+) 0 sqr
	divide (FloatVec vec) sca =
		FloatVec $ map (\x -> x / sca) vec
	zero i =
		FloatVec v where	
			v = map (\x -> 0.0) [1..i]

instance Vec WordVec where
	mag (WordVec a s) = 
		let sqr = M.map (\v -> v*v) a in
		let (sum,_) = M.mapAccum (\a b-> (a+b,b)) 0 sqr in
		sum
	divide (WordVec a s) sca =
		WordVec (M.map (\v-> v / sca) a) s
	zero i =
		WordVec M.empty "empty"

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

removeCommonWords :: [String] -> [String]
removeCommonWords wlst =
	filter (\x -> not $ contains common x) wlst where
		common = ["is","at","and","a","the","of","an","was","were","will","do","but","for","to"]

stringToWordVec :: String -> String -> WordVec
stringToWordVec title str =
	WordVec (wordsToMap str) title


numsToFloatVecs :: [[Float]] -> [FloatVec]
numsToFloatVecs nums = 
	map (\f -> FloatVec f) nums 

fileToFileList :: FilePath -> IO [String]
fileToFileList f = do
	str <- readFile' f
	return $ lines str

--	bracket (openFile f ReadMode) hClose 
--	(\h -> do
--		str <- hGetContents h
--		putStrLn str
--		return $ lines str)

fileToWordVec :: FilePath -> IO WordVec
fileToWordVec fp = do
	str <- readFile' fp
	return $ stringToWordVec fp str

--	bracket (openFile fp ReadMode) hClose 
--	(\h -> do
--		str <- hGetContents h
--		putStrLn str
--		return $ stringToWordVec fp str)

fileListToVecs :: FilePath -> IO [WordVec]
fileListToVecs p = do
	lst <- fileToFileList p
	putStrLn "-- Beginning to Read Files --"
	mapM (\h -> do { putStrLn h ; fileToWordVec h }) lst

intToFloat :: Int -> Float
intToFloat n = fromInteger $ toInteger n

insertPairList :: (Ord a) => [(a,b)] -> M.Map a b
insertPairList lst =
	foldr (\(a,b) acc -> M.insert a b acc) M.empty lst

wordsToMap :: String -> M.Map String Float
wordsToMap str =
	insertPairList $ freqList $ removeCommonWords $ words str

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

groupNearest :: (Vec a) => [a] -> [a] -> [[a]]
groupNearest points means =
	let dists = map
		(\x -> (x, map (\y -> mag $ x - y) means))
		points in
	let clusters = map
		(\i -> filter 
			(\(_,meanlst) -> leastElem meanlst $ meanlst !! i)
			dists) [0..((genericLength means) - 1)] in
	map (\el -> map (\(v,mlst) -> v) el) clusters

findMean :: (Vec a) => [[a]] -> [a]
findMean points =
	map (\group ->
			let sum = foldr (\x acc -> x + acc) (zero shortlst) group in
			divide sum $ genericLength group)
		points where
	shortlst = shortest points

stop_now :: (Eq a) => [[a]] -> [[a]] -> Bool
stop_now a b = a == b

loop_means :: (Vec a) => [a] -> [[a]] -> Int -> [[a]]
loop_means points old k =
	let means = findMean old in
	let next = groupNearest points means in
	case stop_now old next of
		True -> next
		False -> loop_means points next k 
	

kmeans :: (Vec a) => [a] -> Int -> IO [[a]]
kmeans points k = do
	shuffed <- shuffle points
	let means = take k shuffed
	let begin = groupNearest points means
	return $ loop_means points begin k 	

runOnFile fileName num = do
	veclst <- fileListToVecs fileName
	putStrLn "-- Finished Reading Files --"
	out <- kmeans veclst num
	mapM (\i -> do
		putStrLn $ "!!!  " ++ (show i) ++ "  !!!"	
		putStrLn $ show $ out !! i)
		[0..(num-1)]
