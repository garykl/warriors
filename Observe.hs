module Observe where


import qualified Warrior as W
import qualified Logic as L
import Geometry
import qualified Data.Map as M
import Data.Functor ((<$>))
import Control.Applicative
import Data.List (minimumBy, maximumBy)
import Data.Function (on)


data FeatureExtraction a = FE (L.Environment -> a)


within :: FeatureExtraction a -> L.Environment -> a
within (FE f) = f


instance Functor FeatureExtraction where
    fmap f (FE fef) = FE (f . fef)


instance Applicative FeatureExtraction where
    pure a = FE (const a)
    (FE f) <*> (FE a) = FE (\env -> f env (a env))


-- I guess this is not useful
-- instance Monad FeatureExtraction where
--     return a = FE (const a)
--     (FE a) >>= f = extract $ f . a
--       where extract :: (L.Environment -> FeatureExtraction a)
--                     -> FeatureExtraction a
--             extract fe = fe (L.Env M.empty [])


onlyLiving :: [W.Warrior] -> [W.Warrior]
onlyLiving = filter (not . W.warriorDead)

enemies :: FeatureExtraction [W.Warrior]
enemies = FE $ \(L.Env _ tribes) -> onlyLiving $ concatMap M.elems tribes


friends :: FeatureExtraction [W.Warrior]
friends = FE $ \(L.Env tribe _) -> onlyLiving $ M.elems tribe


wherestCharacter :: ((W.Warrior -> W.Warrior -> Ordering) -> [W.Warrior] -> W.Warrior)
                 -> FeatureExtraction [W.Warrior]
                 -> W.Warrior -> FeatureExtraction (Maybe W.Warrior)
wherestCharacter g few warrior =

    maybeWhereBy g (compare `on` vectorLength
                             . W.liftAgents W.agentToAgentVector warrior)
        <$> few


maybeWhereBy :: ((a -> a -> Ordering) -> [a] -> a)
             -> (a -> a -> Ordering) -> [a] -> Maybe a
maybeWhereBy wby f ll = if null ll then Nothing else Just $ wby f ll


-- | return the relative position of the nearest enemy
nearestEnemy :: W.Warrior -> FeatureExtraction (Maybe W.Warrior)
nearestEnemy = wherestCharacter minimumBy enemies


-- | return the relative position of the farest friend
farestFriend :: W.Warrior -> FeatureExtraction (Maybe W.Warrior)
farestFriend = wherestCharacter maximumBy friends


-- | return the relative position of the farest enemy
farestEnemy :: W.Warrior -> FeatureExtraction (Maybe W.Warrior)
farestEnemy = wherestCharacter maximumBy enemies


-- | return the relative position of the nearest friend
bearestFriend :: W.Warrior -> FeatureExtraction (Maybe W.Warrior)
bearestFriend = wherestCharacter minimumBy friends
