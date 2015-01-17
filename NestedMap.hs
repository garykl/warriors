module NestedMap where


import Prelude hiding (map, zipWith, unzip)
import qualified Data.Map as M
import qualified Data.List as L


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


values :: (Ord a, Ord b) => Nmap a b c -> [c]
values nmap = L.map (nmap !) $ keys nmap


keysFromMap :: Ord a =>  M.Map a [b] -> [(a, b)]
keysFromMap m =
    let outerkeys = M.keys m
    in  concatMap (\o -> let is = m M.! o
                         in  zip (replicate (length is) o) is) outerkeys



elems :: Nmap a b c -> [M.Map b c]
elems (Nmap nmap) = M.elems nmap

elemsDeep :: (Ord a, Ord b) => Nmap a b c -> [c]
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
    in  Nmap $ if M.null sub then M.delete a nmap
                             else M.insert a sub nmap


deleteRough :: (Ord a) => a -> Nmap a b c -> Nmap a b c
deleteRough key (Nmap nmap) = Nmap $ M.delete key nmap


-- | combine the elements of two nested maps by a function. If a key does only
-- exist in one the maps, the result will not contain that key.
zipWith :: (Ord a, Ord b) => (c -> d -> e)
        -> Nmap a b c -> Nmap a b d -> Nmap a b e
zipWith f (Nmap n1) (Nmap n2) =

    let outerkeys = M.keys n1 `L.intersect` M.keys n2
    in  Nmap . M.fromList
            $ L.map (\o -> (o, innerZipWith (n1 M.! o) (n2 M.! o))) outerkeys

  where innerZipWith m1 m2 =
            let innerkeys = M.keys m1 `L.intersect` M.keys m2
            in  M.fromList $ L.map (\i -> (i, f (m1 M.! i) (m2 M.! i))) innerkeys


unzip :: (Ord a, Ord b) => Nmap a b (c, d) -> (Nmap a b c, Nmap a b d)
unzip nmap = (map fst nmap, map snd nmap)


map :: (Ord a, Ord b) => (c -> d) -> Nmap a b c -> Nmap a b d
map f nmap =
    let ks = keys nmap
        vs = L.map (f . (!) nmap) ks
    in  foldr (.) id (L.zipWith (-<-) ks vs) empty
