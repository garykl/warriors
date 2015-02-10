module Observe where


import qualified Warrior as W
import qualified Logic as L
import Geometry
import qualified Data.Map as M
import Data.Functor ((<$>))


data FeatureExtraction a = FE (L.Environment -> a)


instance Functor FeatureExtraction where
    -- fmap :: (a -> b) -> FeatureExtraction a -> FeatureExtraction b
    fmap f (FE fef) = FE (f . fef)


enemies :: FeatureExtraction [W.Warrior]
enemies = FE $ \(L.Env _ tribes) -> concatMap M.elems tribes


friends :: FeatureExtraction [W.Warrior]
friends = FE $ \(L.Env tribe _) -> M.elems tribe


wherestCharacter :: ([Vector] -> Vector)
                 -> FeatureExtraction [W.Warrior]
                 -> W.Warrior -> FeatureExtraction Vector
wherestCharacter g few warrior =
    g <$> map (W.liftAgents W.agentToAgentVector warrior) <$> few


-- | return the relative position of the nearest enemy
nearestEnemy :: W.Warrior -> FeatureExtraction Vector
nearestEnemy = wherestCharacter shortestVector enemies


-- | return the relative position of the farest friend
farestFriend :: W.Warrior -> FeatureExtraction Vector
farestFriend = wherestCharacter longestVector friends


-- | return the relative position of the farest enemy
farestEnemy :: W.Warrior -> FeatureExtraction Vector
farestEnemy = wherestCharacter longestVector enemies


-- | return the relative position of the nearest friend
bearestFriend :: W.Warrior -> FeatureExtraction Vector
bearestFriend = wherestCharacter shortestVector friends
