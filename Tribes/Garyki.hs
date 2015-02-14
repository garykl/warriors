module Tribes.Garyki where


import KiBuild
import Geometry
import qualified Observe as O
import qualified Data.Map as M


mover :: Vector -> Intelligence
mover vec _ _ = move vec


attacker :: Intelligence
attacker warrior env =
    let enemy = O.nearestEnemy warrior `O.within` env
    in  case enemy of
            Nothing -> move (Vec 0 500)
            Just enem ->
              if beatable warrior
                          (liftAgents agentToAgentVector warrior enem)
                          enem
                then melee $ liftAgents agentToAgentVector warrior enem
                else move $ liftAgents agentToAgentVector warrior enem


warrior :: Position -> Warrior
warrior pos = Warrior
    Soul { figureClass = Wobble,
           size = 30,
           velocity = 3,
           vitality = 50,
           strength = 9 }
    Agent { position = pos,
            lifepoints = 50,
            actionStatus = Faineancing }


intelligentWarrior :: Position -> (Warrior, Intelligence)
intelligentWarrior pos@(Pos x y) =
    (warrior pos, attacker)


(-<-) :: Ord k => k -> a -> M.Map k a -> M.Map k a
key -<- value = M.insert key value


provide :: M.Map WarriorName (Warrior, Intelligence)
provide = ("Gerhardt" -<- intelligentWarrior (Pos 500 0))
        . ("Gustav" -<- intelligentWarrior (Pos (-500) 0))
        $ M.empty
