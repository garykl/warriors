module NestedMap where


import qualified Data.Map as M


-- | implement a nested map that is usable as if it is a map of tuples.
data Nmap a b c = Nmap (M.Map a (M.Map b c)) deriving Show


-- | initialize a nested map
empty :: Nmap a b c
empty = Nmap M.empty


singleton :: (Ord a, Ord b) => a -> b -> c -> Nmap a b c
singleton a b c = c ->- (a, b) $ empty


-- | read from nested map
(!) :: (Ord a, Ord b) => Nmap a b c -> (a, b) -> c
Nmap c ! (a, b) = c M.! a M.! b

elem :: (Ord a) => a -> Nmap a b c -> M.Map b c
elem key (Nmap nmap) = nmap M.! key


keysRough :: Nmap a b c -> [a]
keysRough (Nmap nmap) = M.keys nmap


keys :: (Ord a, Ord b) => Nmap a b c -> [(a, b)]
keys (Nmap nmap) = concatMap (\t -> constantZip t $ M.keys $ nmap M.! t)
                           $ M.keys nmap
  where constantZip :: b -> [a] -> [(b, a)]
        constantZip n ll = zip (replicate (length ll) n) ll


elems :: Nmap a b c -> [M.Map b c]
elems (Nmap nmap) = M.elems nmap

elemsDeep :: (Ord a) => Nmap a b c -> [c]
elemsDeep (Nmap nmap) = concatMap M.elems $ M.elems nmap


-- | insert into nested map
insert :: (Ord a, Ord b) => (a, b) -> c -> Nmap a b c -> Nmap a b c
insert key value (Nmap nmap) =
    let submap = if M.member (fst key) nmap then nmap M.! fst key else M.empty
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



-- | delete
delete :: (Ord a, Ord b) => (a, b) -> Nmap a b c -> Nmap a b c
delete (a, b) (Nmap nmap) =
    let sub = M.delete b $ nmap M.! a
    in  Nmap $ M.insert a sub nmap


deleteRough :: (Ord a) => a -> Nmap a b c -> Nmap a b c
deleteRough key (Nmap nmap) = Nmap $ M.delete key nmap
