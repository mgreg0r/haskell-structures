module Graph where
import Set(Set)
import qualified Set as Set
import Data.Semigroup
import Data.List(nub)

class Graph g where
  empty   :: g a
  vertex  :: a -> g a
  union   :: g a -> g a -> g a
  connect :: g a -> g a -> g a

data Relation a = Relation { domain :: Set a, relation :: Set (a, a) }
    deriving (Eq, Show)

data Basic a = Empty
             | Vertex a
             | Union (Basic a) (Basic a)
             | Connect (Basic a) (Basic a)

instance Graph Relation where
  empty = Relation Set.empty Set.empty
  vertex v = Relation (Set.singleton v) Set.empty
  union (Relation v1 e1) (Relation v2 e2) = Relation (Set.union v1 v2) (Set.union e1 e2)
  connect (Relation v1 e1) (Relation v2 e2) = Relation (Set.union v1 v2) (Set.union (Set.union e1 e2) (Set.fromList [(x, y) | x <- Set.toList v1, y <- Set.toList v2]))

instance (Ord a, Num a) => Num (Relation a) where
  fromInteger = vertex . fromInteger
  (+)         = union
  (*)         = connect
  signum      = const empty
  abs         = id
  negate      = id

instance Graph Basic where
  empty = Empty
  vertex v = Vertex v
  union l r = Union l r
  connect l r = Connect l r

instance Ord a => Eq (Basic a) where
  l == r = v1 == v2 && e1 == e2 where
    (Relation v1 e1) = fromBasic l
    (Relation v2 e2) = fromBasic r

instance (Ord a, Num a) => Num (Basic a) where
  fromInteger = vertex . fromInteger
  (+)         = union
  (*)         = connect
  signum      = const empty
  abs         = id
  negate      = id

instance Semigroup (Basic a) where
  (<>) = union

instance Monoid (Basic a) where
  mempty = Empty

fromBasic :: Graph g => Basic a -> g a
fromBasic Empty = empty
fromBasic (Vertex v) = vertex v
fromBasic (Union l r) = union (fromBasic l) (fromBasic r)
fromBasic (Connect l r) = connect (fromBasic l) (fromBasic r)

containsVertex :: Ord a => a -> [(a, a)] -> Bool
containsVertex v [] = False
containsVertex v ((l, r):t) = if v == l || v == r then True else containsVertex v t

printableLists :: Ord a => Basic a -> ([a], [(a, a)])
printableLists b = (filteredVList, eList) where
  vList = nub (Set.toAscList v)
  eList = nub (Set.toAscList e)
  (Relation v e) = fromBasic b
  filteredVList = filter (\x -> not (containsVertex x eList)) vList

instance (Ord a, Show a) => Show (Basic a) where
  show b = "edges " ++ (show e) ++ " + vertices " ++ (show v) where
    (v, e) = printableLists b

-- | Example graph
-- >>> example34
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

example34 :: Basic Int
example34 = 1*2 + 2*(3+4) + (3+4)*5 + 17

printEdge :: Show a => String -> (a, a) -> String
printEdge acc (l, r) = acc ++ (show l) ++ " -> " ++ (show r) ++ ";"

printVertex :: Show a => String -> a -> String
printVertex acc v = acc ++ (show v) ++ ";"

todot :: (Ord a, Show a) => Basic a -> String
todot b = "digraph {" ++ (foldl printEdge "" e) ++ (foldl printVertex "" v) ++ "}" where
  (v, e) = printableLists b

instance Functor Basic where
  fmap f Empty = Empty
  fmap f (Vertex v) = Vertex (f v)
  fmap f (Union l r) = Union (fmap f l) (fmap f r)
  fmap f (Connect l r) = Connect (fmap f l) (fmap f r)

-- | Merge vertices
-- >>> mergeV 3 4 34 example34
-- edges [(1,2),(2,34),(34,5)] + vertices [17]

mergeV :: Eq a => a -> a -> a -> Basic a -> Basic a
mergeV a b c Empty = Empty
mergeV a b c (Vertex v) = if a == v || b == v then Vertex c else Vertex v
mergeV a b c (Union l r) = Union (mergeV a b c l) (mergeV a b c r)
mergeV a b c (Connect l r) = Connect (mergeV a b c l) (mergeV a b c r)

instance Applicative Basic where
  pure = vertex
  (Vertex f) <*> (Vertex x) = Vertex (f x)
  (Union l1 r1) <*> (Union l2 r2) = Union (l1 <*> l2) (r1 <*> r2)
  (Connect l1 r1) <*> (Connect l2 r2) = Connect (l1 <*> l2) (r1 <*> r2)
  _ <*> _ = Empty 


instance Monad Basic where
  Empty >>= k = Empty
  (Vertex v) >>= k = k v
  (Union l r) >>= k = Union (l >>= k) (r >>= k)
  (Connect l r) >>= k = Connect (l >>= k) (r >>= k)

-- | Split Vertex
-- >>> splitV 34 3 4 (mergeV 3 4 34 example34)
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

splitV :: Eq a => a -> a -> a -> Basic a -> Basic a
splitV a b c g = g >>= (\x -> if x == a then Union (Vertex b) (Vertex c) else Vertex x)

