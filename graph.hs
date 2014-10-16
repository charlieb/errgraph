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

shortest_path :: Graph -> ErrorCode -> ErrorCode -> [ErrorCode]
shortest_path g a b = shortest_path2 g a b []
--distances_from_node :: Graph -> Ord -> [(ErrorCode, Int)]

test = shortest_path (windows_to_graph [[1,2,3],[3,4,5],[1,4,6]]) 4 3

-- To make easy for now, limit so we can always travel the shortest path to next node
shortest_path2 g a b visited
    | null kids = [] -- maybe sb [a]
    | a == b = reverse $ a:visited
    | otherwise = check_paths g a b visited kids
    where kids = filter (\ c -> not $ elem c visited) $ child_codes g a

check_paths g a b visited candidates
    | null candidates = []
    | null shortest = check_paths g a b visited (tail candidates)
    | otherwise = shortest
    where shortest = shortest_path2 g (head candidates) b (a:visited)
