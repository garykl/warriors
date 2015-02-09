module Game where


import Warrior
import Logic
import qualified Observe as O
import qualified NestedMap as N
import Geometry
import Rand as R

import Debug


-- | for knowing what the @Effect@ of an @Action@ is, we need the @Field@,
-- the @Warrior@ tht performs the @Action@ and the @Action@ itself. Multiple
-- target may be affected (-> keys are @WarriorIdentifier@s) and the effect
-- may be in the future (-> values are lists of @Effect@s).
actionToEffects :: Field -> WarriorIdentifier -> Action -> Effects
actionToEffects field wid action =
    let warrior@(Warrior soul agent) = field N.! wid
    in
      if warriorDead warrior then emptyEffects (N.keys field) else
        case action of

            Melee vec ->

                let status = actionStatus agent
                    (newStatus, phase) = case status of
                        MeleeAttacking amount phase target ->
                            let newPhase = (1 + phase) `mod` meleeDuration
                            in  (MeleeAttacking amount newPhase vec, newPhase)
                        _ ->
                            let newPhase = meleeDuration `div` 2
                            in  (MeleeAttacking 1 newPhase vec, newPhase)

                    -- find nearest warrior
                    pos = position agent .+ vec

                    nearestIds = O.warriorDistances pos field
                    nearestPoss =
                        map (liftWarrior position . (field N.!)) nearestIds
                    distances = [vectorLength $ np .-. position agent
                                    | np <- nearestPoss]

                    targets =
                        map fst $ filter (\(i, d) -> d < meleeDistance)
                                       $ zip nearestIds (debug distances)

                    -- produce effect templates
                    attackerEffect ag = ag { actionStatus = newStatus }
                    defenderEffect ag
                      | agentDead ag = ag
                      | otherwise =
                          if phase == 0
                            then ag {lifepoints = lifepoints ag - meleeDamage}
                            else ag

                -- compose effects
                in  ([Effect attackerEffect] N.->- wid) .
                    composeAll [[Effect defenderEffect] N.->- ni | ni <- targets]
                        $ emptyEffects $ N.keys field


            Move vec ->

                -- compute what future could bring
                let direction = normalize vec
                    futurePosition =
                        R.randomize (Pos 1 1)
                                $ position agent .+ (velocity soul |*| direction)
                    futureAgent = agent { position = futurePosition }
                    effect ag = ag {
                        position = futurePosition,
                        actionStatus = Moving direction }

                -- check if future is possible and make it eventually
                in  if O.fieldCross (N.delete wid field)
                                    (Warrior soul futureAgent)
                      then emptyEffects $ N.keys field
                      else [Effect effect] N.->- wid $ emptyEffects $ N.keys field



chooseAndPerformAction :: WarriorIdentifier -> Intelligences
                       -> Effects -> Field -> (Effects, Field)
chooseAndPerformAction wid intelligences effects field =
    let action = chooseAction wid intelligences field
        newEffects = actionToEffects field wid action
    in  applyEffects (composeEffects effects newEffects) field


-- | each @Warrior@ on the @Field@ can try to act appropriately.
performTimestep :: Intelligences -> Field -> Field
performTimestep intelligences field =
    let performs = map (\wid -> chooseAndPerformAction
                                    wid
                                    intelligences
                                    (emptyEffects (N.keys field)))
                     $ getOrder field
    in  composeAllSnd performs field
