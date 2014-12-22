module NestedMap where


import qualified Data.Map as M


-- | implement a nested map that is usable as if it is a map of tuples.
data Nmap a b c = Nmap (M.Map a (M.Map b c))


-- | initialize a nested map
empty :: Nmap a b c
empty = Nmap M.empty


-- | read from nested map
(!) :: (Ord a, Ord b) => Nmap a b c -> (a, b) -> c
Nmap c ! (a, b) = c M.! a M.! b


-- | insert into nested map
insert :: (Ord a, Ord b) => (a, b) -> c -> Nmap a b c -> Nmap a b c
insert key value (Nmap nmap) =
    let submap = nmap M.! fst key
        updsub = M.insert (snd key) value submap
    in  Nmap $ M.insert (fst key) updsub nmap


-- | insert into nested map
(->-) :: (Ord a, Ord b) => c -> (a, b) -> Nmap a b c -> Nmap a b c
value ->- key = insert key value

-- | insert into nested map
(-<-) :: (Ord a, Ord b) => (a, b) -> c -> Nmap a b c -> Nmap a b c
key -<- value = value ->- key


-- | modify a nested map
modify :: (Ord a, Ord b) => (a, b) -> (c -> c) -> Nmap a b c -> Nmap a b c
modify key f nmap =
    let value = nmap ! key
    in  f value ->- key $ nmap


-- | modify a nested map
(~>~) :: (Ord a, Ord b) => (c -> c) -> (a, b) -> Nmap a b c -> Nmap a b c
f ~>~ key = modify key f

-- | modify a nested map
(~<~) :: (Ord a, Ord b) => (a, b) -> (c -> c) -> Nmap a b c -> Nmap a b c
key ~<~ f = f ~>~ key

