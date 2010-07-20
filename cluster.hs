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

intToFloat :: Int -> Float
intToFloat n = fromInteger $ toInteger n

leastElem :: (Ord a) => [a] -> a -> Bool
leastElem lst el =
	filter (\x -> x < el) lst == [] 

groupNearest :: (Ord a,Num a) => [Vec a] -> [Mean a] -> [Cluster a]
groupNearest points means =
	let dists = map
		(\x -> (x, map (\y -> mag $ x - y) means))
		points in
	let clusters = map
		(\i -> filter 
			(\(_,meanlst) -> leastElem meanlst $ meanlst !! i)
			dists) [0..((length means) - 1)] in
	map (\el -> map (\(v,mlst) -> v) el) clusters

findMean :: [Cluster Float] -> [Mean Float]
findMean points =
	map (\group ->
			let sum = foldr (\x acc -> x + acc) (Vec [0.0,0.0,0.0]) group in
			divide sum $ intToFloat (length group))
		points

