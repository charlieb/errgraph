-- List of error codes -> a window
data Edge = Edge Integer Integer Integer deriving Show
--data Path = [Ord]

inc_edge :: [Edge] -> Integer -> Integer -> [Edge]
inc_edge [] x y = [Edge x y 1]
inc_edge (g@(Edge a b n):gs) x y
    | (a == x && b == y) || (a == x && b == y) = Edge a b (n+1):gs
    | otherwise = (Edge a b n: (inc_edge gs x y))
--window_to_edges :: [[Ord]] -> Graph
--window_to_edges [] = []
--window_to_egdes (w:ws) = 

--children :: Graph -> Ord -> Graph
--shortest_path :: Graph -> Ord -> Ord -> Path
--istances_from_node :: Graph -> Ord -> [(Ord, Int)]
