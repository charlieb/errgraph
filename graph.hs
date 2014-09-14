-- List of error codes -> a window
type ErrorCode = Integer
data Edge = Edge Integer Integer Integer deriving Show
type Graph = [Edge]
--data Path = [Ord]

inc_edge :: Graph -> ErrorCode -> ErrorCode -> Graph 
inc_edge [] x y = [Edge x y 1]
inc_edge (g@(Edge a b n):gs) x y
    | (a == x && b == y) || (a == x && b == y) = Edge a b (n+1):gs
    | otherwise = (Edge a b n: (inc_edge gs x y))

rmdups :: [Integer] -> [Integer]
rmdups (x:xs)
  | xs == [] = [x]
  | elem x xs = rmdups xs
  | otherwise = x:(rmdups xs)

pairs :: [Integer] -> [(Integer, Integer)]
pairs [x] = []
pairs (x:xs) = map (\ y -> (x,y)) xs ++ pairs xs

windows_to_graph :: [[ErrorCode]] -> Graph
windows_to_graph ws = foldl (\ graph (x, y) -> inc_edge graph x y) [] $ concatMap (pairs . rmdups) ws

children :: Graph -> ErrorCode -> Graph
children g er = filter (\ (Edge a b _) -> a == er || b == er) g

all_error_codes :: [Edge] -> [ErrorCode]
all_error_codes es = rmdups $ concatMap (\ (Edge a b _) -> [a,b]) es

child_codes :: Graph -> ErrorCode -> [ErrorCode]
child_codes g er = filter (/= er) $ all_error_codes $ children g er

--shortest_path :: Graph -> Ord -> Ord -> Path
--istances_from_node :: Graph -> Ord -> [(Ord, Int)]
