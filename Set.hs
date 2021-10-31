module Set(Set(..), empty, null, singleton, union, fromList
              , member, toList, toAscList, elems
              ) where
import Prelude hiding(null)
import Data.List(sort, nub)
import Data.Semigroup

data Set a = Empty
           | Singleton a
           | Union (Set a) (Set a)

empty :: Set a
empty = Empty

null :: Set a -> Bool
null Empty = True
null _ = False

member :: Eq a => a -> Set a -> Bool
member _ Empty = False
member a (Singleton e) = a == e
member a (Union l r) = (member a l) || (member a r)

singleton :: a -> Set a
singleton x = Singleton x

fromList :: [a] -> Set a
fromList [] = Empty
fromList (h:t) = Union (Singleton h) (fromList(t))

toList :: Set a -> [a]
toList Empty = []
toList (Singleton x) = [x]
toList (Union l r) = toList(l) ++ toList(r)

toAscList :: Ord a => Set a -> [a]
toAscList s = sort (toList s)

elems :: Set a -> [a]
elems = toList

union :: Set a -> Set a -> Set a
union Empty Empty = Empty
union Empty r = r
union l Empty = l
union l r = Union l r

insert :: a -> Set a -> Set a
insert a Empty = Singleton a
insert a s = Union (Singleton a) s

instance Ord a => Eq (Set a) where
  x == y = nub (toAscList x) == nub (toAscList y)

instance Semigroup (Set a) where
  (<>) = union

instance Monoid (Set a) where
  mempty = Empty

instance Show a => Show (Set a) where
  show Empty = "[]"
  show (Singleton x) = "[" ++ (show x) ++ "]"
  show s = show (toList s)

instance Functor Set where
  fmap f Empty = Empty
  fmap f (Singleton a) = Singleton (f a)
  fmap f (Union l r) = Union (fmap f l) (fmap f r)

