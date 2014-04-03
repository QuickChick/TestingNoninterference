{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, RankNTypes #-}

-- Common definitions for generic "machines"

module GenericMachine where

import Control.Applicative
import Control.Monad
import Test.QuickCheck
import Data.Function
import Trace

-- Typeclass of abstract machines.  Note that the "single step
-- relation" is implemented in two ways: (1) As a random *generator*
-- for next states (step) and (2) as a *predicate* testing whether two
-- states are related.  This is a standard QC trick for dealing with
-- sets.  We need (1) so that we can quantify over the elements of the
-- set (of pairs of related states), and (2) so that we can test for
-- membership in the set (relatedness).  We also need a notion of
-- well-formedness, which intuitively means "This machine is not
-- (right now) in a bad state."  The reason for this is that
-- generators can only represent nonempty sets.  The wf predicate
-- identifies the states that are able to take one next step.
-- 
-- (Note that if we didn't care about nondeterminism we could get away
-- with just the function.  This might also allow us to simplify away
-- the wf predicate, though we'd need to reimplement quite a bit of it
-- elsewhere...)

data WFCheck
  = WF | IF { illf_reason :: String }
  deriving (Eq, Show)

orElse :: Bool -> String -> WFCheck
orElse True _       = WF
orElse False reason = IF { illf_reason = reason }

infix 3 `orElse`

wfChecks :: [WFCheck] -> WFCheck
-- Implements the conjuction of all these checks, leftmost first.
wfChecks [] = WF
wfChecks ((IF reason):_) = IF reason
wfChecks (c:cs) = wfChecks cs

class Show s => Machine s where
  isStep :: s -> s -> Bool
  step   :: s -> Gen s
  wf     :: s -> WFCheck
  -- illf   :: s -> Maybe String
  -- wf     :: s -> Bool
  
  -- illf s | wf s      = Nothing
  --        | otherwise = Just "not well-formed"  
  -- wf = isNothing . illf

isWF :: Machine s => s -> Bool
isWF s | WF <- wf s = True
       | otherwise  = False

defaultStep :: Machine s => String -> (s -> Gen s) -> s -> Gen s
defaultStep who fn s =
  case wf s of
    WF     -> fn s
    IF err -> error $  "step for " ++ who ++ " received an ill-formed state: "
                    ++ err

-- -- Three operators for implementing illf

-- (??) :: Alternative f => Bool -> a -> f a
-- True  ?? _ = empty
-- False ?? a = pure a
-- infix 3 ??

-- (||||) :: Alternative f => Bool -> f a -> f a
-- True  |||| _ = empty
-- False |||| a = a
-- infixr 2 ||||

-- Needs a different precendence than (<|>).  Also, for our usage, it's
-- semantically more like a conjuction than a disjunction, so this name is
-- nicer.
(&&&&) :: Alternative f => f a -> f a -> f a
(&&&&) = (<|>)
infixr 2 &&&&

stepUntil' :: Machine s => Int -> (s -> s -> Bool) -> s -> s -> Gen s
stepUntil' 0 _ _ s1  = return s1
stepUntil' n p s0 s1
  | p s0 s1       = return s1
  | not $ isWF s1 = return s1
  | otherwise     = stepUntil' (n - 1) p s1 =<< step s1

stepUntil :: Machine s => Int -> (s -> s -> Bool) -> s -> Gen s
stepUntil n p s | isWF s = stepUntil' (n - 1) p s =<< step s
stepUntil n p s = return s

isSteps :: Machine s => [s] -> Bool
isSteps css =
  and $ zipWith isStep css (drop 1 css)

stepMany :: Machine s => s -> Gen s
stepMany cs = 
  if isWF cs then stepMany =<< step cs
  else return cs

stepN :: (Machine s, Monad m) => (s -> m (Maybe s)) -> s -> Int -> m [s]
stepN st s 0 = return [s]
stepN st s n = 
  if isWF s then do
    ms <- st s
    case ms of
      Nothing -> return [s]
      Just s' -> liftM (s :) $ stepN st s' (n-1)
  else return [s]

traceN :: Machine s => s -> Int -> Gen (Trace s)
traceN cs n = Trace <$> stepN (liftM Just . step) cs n

traceNAbs :: Layer cs as => Int -> cs -> Int -> Gen (Trace cs)
traceNAbs maxCN cs n = Trace <$> stepN (stepUntilAbstractable maxCN) cs n

-- Typeclass of a layer between two abstract machines.
-- (Note that as determines cs.  This is a little ugly, but it avoids
-- underdetermined type variables.)
class (Machine cs, Machine as) => Layer cs as | as -> cs, cs -> as where
  -- This could conceptually be a generator, but at the moment it's used as a
  -- function in the implementation of isStep and wf for the abstract layer to
  -- avoid code duplication.
  concretize :: as -> cs

  -- This is a Maybe so that we can decide when a run of the concrete machine
  -- has taken a single step (namely when it reaches the first abstractable
  -- state). It does not guarantee that the resulting state is wf.
  -- (OLD COMMENT?)
  abstract :: cs -> as

  -- Abstract should always succeed, but it might return the last state of the
  -- abstract machine. A state is abstractable if it actually corresponds to an
  -- abstract state.
  abstractable :: cs -> Bool

  -- Typically an abstract step is finished when the machine is in an
  -- abstractable state, but this isn't necessarily true. For instance, a
  -- multicore machine is abstractable if any of the cores are in an
  -- abstractable state, but it doesn't finish an abstract step unless one of
  -- the cores steps into an abstractable state.
  finishedAbstractStep :: cs -> cs -> Bool
  finishedAbstractStep cs0 cs1 = abstractable cs1

stepUntilAbstractable :: Layer cs as => Int -> cs -> Gen (Maybe cs)
stepUntilAbstractable maxCSteps cs
  | isWF as   = Just <$> stepUntil maxCSteps finishedAbstractStep cs
  | otherwise = return Nothing
  where
    as = abstract cs

-- Generic properties -----------------------------------------------------

prop_steps :: Machine s => s -> Property
prop_steps s =
  forAll (stepN ((Just <$>) . step) s 100) $
    collect <$> (wf . last) <*> isSteps

-- There's no good way to test `abstractable cs ==> concretize (abstract cs) ==
-- cs`, because it's false---abstracting loses information.  And applying
-- abstract to both sides of the equality just produces a special case of
-- prop_roundtrip.
prop_roundtrip :: (Layer cs as, Eq as) => as -> Bool
prop_roundtrip as = abstract (concretize as) == as

-- TODO: These need to participate in the testing infrastructure
-- (maybe there should be an entry point for running them as part of
-- the Layer abstraction?)

-- TODO: Rename this to something more sensible!!
-- First arguments is max number of concrete steps per abstract step, and
-- second argument is max number of abstract steps for each configuration.
gen_prop_correct :: (Layer cs as, Testable prop, Arbitrary as) =>
                    Int -> Int -> Gen as ->
                    (forall prop' . Testable prop' => [cs] -> [as] -> prop' -> prop) ->
                    Property
gen_prop_correct maxCSteps maxASteps gas f =
  forAllShrink gas shrink $ \as ->
  isWF as ==> 
  let cs = concretize as in
  -- Quantify over all concrete executions that run for at most
  -- maxASteps steps of a step function that steps the concrete
  -- machine until it reaches an abstractable state.  (The Blind makes
  -- it not show the full sequence of concrete steps in
  -- un-prettyprinted form.)
  forAll (Blind <$> stepN (stepUntilAbstractable maxCSteps) cs maxASteps) $ \(Blind css) ->
    let ok  = all abstractable css
        ass = map abstract css
    in f css ass (ok && and [ not (isWF cs1) || (isStep `on` abstract) cs0 cs1
                            | (cs0, cs1) <- zip css (drop 1 css) ])

