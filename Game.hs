module Game where


import Logic
import qualified Observe as O
import qualified NestedMap as N
import Geometry
import Rand as R

-- | for knowing what the @Effect@ of an @Action@ is, we need the @Field@,
-- the @Warrior@ tht performs the @Action@ and the @Action@ itself. Multiple
-- target may be affected (-> keys are @WarriorIdentifier@s) and the effect
-- may be in the future (-> values are lists of @Effect@s).
actionToEffects :: Field -> WarriorIdentifier -> Action -> Effects
actionToEffects field wid action =
    let warrior@(Warrior soul agent) = field N.! wid
    in
      if O.warriorDead warrior then emptyEffects (N.keys field) else
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
                    nearestId = head $ O.warriorDistances pos field
                    nearestWarrior = field N.! nearestId
                    nearestPos = let Warrior _ ag = nearestWarrior
                                 in  position ag

                    distance = vectorLength $ nearestPos .-. position agent

                    attackerEffect ag = ag { actionStatus = newStatus }
                    defenderEffect ag
                      | O.warriorDead nearestWarrior  = ag
                      | otherwise =
                          if distance < meleeDistance && phase == 0
                            then ag {lifepoints = lifepoints ag - meleeDamage}
                            else ag

                    -- check if that warrior is near enough
                in  ([Effect attackerEffect] N.->- wid) .
                    ([Effect defenderEffect] N.->- nearestId)
                        $ emptyEffects $ N.keys field


            MoveTo pos ->

                -- compute what future could bring
                let direction = normalize $ pos .-. position agent
                    futurePosition =
                        R.randomize (Pos 1 1)
                                $ position agent .+ (velocity soul |*| direction)
                    futureAgent = agent { position = futurePosition }
                    effect ag = ag {
                        position = futurePosition,
                        actionStatus = Moving }

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
