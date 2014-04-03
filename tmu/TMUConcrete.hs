{-# LANGUAGE ImplicitParams, FlexibleContexts, UndecidableInstances,
    RecordWildCards, TupleSections #-}

module TMUConcrete where

import Test.QuickCheck.Gen
import Test.QuickCheck
import Control.Applicative
import Text.PrettyPrint hiding (int)
import Data.Function
import Data.Maybe
import Data.List
import Control.Arrow hiding ((<+>))

import Util
import Pretty
import Machine

import TMULabels
import TMUFlags
import TMUObservable
import TMUInstr
import TMUTMMRoutine

{---------------------------- The concrete machine ----------------------------}

-- TODO Can we unify CStkElt and AStkElt?  Maybe with a type class?  Probably
-- not.

data CStkElt = CData Atom
             | CRet  (Labeled (Int,Bool)) -- (address to return to,
                                          --  whether or not a value is returned)
                     Bool                 -- the previous privileged bit
  deriving (Eq, Read, Show)  

isCData :: CStkElt -> Bool
isCData (CData _) = True
isCData _         = False

cstkData :: CStkElt -> Atom
cstkData (CData d) = d
cstkData _         = error "cstkData received a CRet"

cstkValue :: CStkElt -> Int
cstkValue (CData d) = value d
cstkValue _         = error "cstkValue received a CRet"

cstkRetPC :: CStkElt -> Atom
cstkRetPC (CRet p _) = fmap fst p
cstkRetPC _          = error "cstkRetPC received a CData"

cstkReturns :: CStkElt -> Bool
cstkReturns (CRet p _) = snd $ value p
cstkReturns _          = error "cstkRetItems received a CData"

cstkRetPriv :: CStkElt -> Bool
cstkRetPriv (CRet _ b) = b
cstkRetPriv _          = error "cstkRetPriv received a CData"

cstkLab :: CStkElt -> Label
cstkLab (CData d)   = lab d
cstkLab (CRet  p _) = lab p

instance Flaggy DynFlags => Observable CStkElt where
  CData d   ~~~ CData d'    = d ~~~ d'
  CRet  p b ~~~ CRet  p' b' = p ~~~ p' && b ~~~ b'
  _         ~~~ _           = False
  
  -- TODO I don't think we should vary the privileged bit, but I could be convinced
  -- otherwise.
  vary (CData d)   = CData <$> vary d
  vary (CRet  p b) = CRet  <$> vary p <*> pure b
  
  shrinkV (Variation (CData d) (CData d')) =
    map (CData <$>) . shrinkV $ Variation d d'
  shrinkV (Variation (CRet p b) (CRet p' b')) | b == b' =
    map (flip CRet b <$>) . shrinkV $ Variation p p'
  shrinkV v = errorShrinkV "CStkElt" v

instance Flaggy DynFlags => Arbitrary CStkElt where
  -- TODO I don't think we should generate privileged return addresses, but I
  -- could be convinced otherwise.
  arbitrary = frequency
            $  [ (4, CData <$> labeled int) ]
            ++ [ (1, CRet  <$> labeled ((,) <$> choose (0,20) <*> arbitrary)
                           <*> pure False)
               | cally ]
    where cally = callsAllowed (gen_instrs getFlags)
  
  shrink (CData i)   = map CData $ shrink i
  shrink (CRet  p b) = CData (fst <$> p) : map (uncurry CRet) (shrink (p,b))

data CS = CS { 
    mem     :: [Atom]       -- Data memory
  , imem    :: [Instr]      -- Instruction memory
  , stk     :: [CStkElt]    -- Stack
  , pc      :: Labeled Int  -- Program counter
  , priv    :: Bool }       -- Privileged bit (are we allowed to run in the TMM?)
  deriving (Eq, Read)

instance Flaggy DynFlags => Pretty CS where
  pretty CS{..} =
    text "CS" <+> record
      [ field "mem"  $ l mem
      , field "imem" $ let (tmmR,imem') = splitAt (length tmmRoutine) imem
                           tmmRName     = text . ("<" ++) . (++ ">") . show
                                        $ which_tmm_routine getFlags
                       in list $ tmmRName : map (text . show) imem'
      , field "stk"  $ l stk
      , field "pc"   $ text $ show pc
      , field "priv" $ text $ show priv ]
    where
      l :: Show a => [a] -> Doc
      l xs = list $ map (text . show) xs
      field s d = sep [ text s <+> text "=", nest 2 d ]

instance Flaggy DynFlags => Show CS where
  show = show . pretty

data PreservingInit a = PreservingInit Int [a] deriving (Eq, Read, Show)

instance Arbitrary a => Arbitrary (PreservingInit a) where
  arbitrary = PreservingInit 0 <$> arbitrary
  shrink (PreservingInit n xs) =
    let (pre,post) = splitAt n xs
    in map (PreservingInit n . (pre ++)) $ shrink post

instance Flaggy DynFlags => Arbitrary CS where
  arbitrary = do
    mem  <- listOf (labeled int)
    imem <- arbitrary
    stk  <- listOf arbitrary
    return CS { mem  = replicate tmuCacheSize (Labeled H (-1)) ++ mem
              , imem = tmmRoutine ++ imem
              , stk  = stk
              , pc   = Labeled L $ length tmmRoutine
              , priv = False}
  -- Don't shrink the privileged bit, and make sure to keep the TMU cache and
  -- the TMM routine.
  shrink CS{..} = 
    [ CS{mem = mem', imem=imem', stk=stk', pc=pc', priv=priv}
    | (PreservingInit _ mem', PreservingInit _ imem', stk', pc') <-
         shrink ( PreservingInit tmuCacheSize mem
                , PreservingInit (length tmmRoutine) imem
                , stk
                , pc )
    , value pc >= length tmmRoutine, value pc' >= length tmmRoutine ]

instance Flaggy DynFlags => Machine CS where
  isStep cs cs' = isWF cs && cs' == stepFn cs
  
  step = defaultStep "TMUConcrete.CS" $ return . stepFn
  
  -- An instruction is well-formed if the current instruction could execute
  -- without a non-label-related error: the stack has enough elements, addresses
  -- aren't out of bounds, we're not inside the TMU, etc.
  wf CS{..}
    = checkTMM . wfChecks $
       isAddr "pc" imem (length tmmRoutine) priv (value pc) ++ -- ASZ: Should we say priv <-> tmmRoutine, not just tmmRoutine -> priv?
       instr_check (imem !! value pc)
    where instr_check Noop       = [WF]
          instr_check Add        = [stackSize 2]
          instr_check Sub        = [stackSize 2]
          instr_check (Push _)   = [WF]
          instr_check Pop        = [stackSize 1]
          instr_check Load       = stackSize 1 : isMAddr top
          instr_check Store      = stackSize 2 : isMAddr top
          instr_check Jump       = [stackSize 1]
          instr_check JumpNZ     = [stackSize 2]
          instr_check (Call a r) = [stackSize $ a+1]
          instr_check (Return _)
            | Just (CRet (Labeled _ (pc',r)) priv') <- find (not . isCData) stk
            = [stackSize $ if r then 1 else 0]
            | otherwise
            = [IF "no return address on stack"]
          instr_check Halt       = [IF "Halt"]
          instr_check LabelOf    = error "instr_check LabelOf not implemented"
          
          checkTMM (IF msg) | priv = IF $ "TMM fault [" ++ msg ++ "]"
          checkTMM wf = wf
          
          stk' = takeWhile isCData stk
          top  = cstkValue $ head stk'
          snd  = cstkValue $ stk' !! 1
          
          stackSize n = (length stk' >= n) `orElse` "stack underflow"
          
          isAddr what arr n p a =
            [ (a `isIndex` arr) `orElse` (what ++ " out of range")
            , (p || a >= n)     `orElse` ("unprivileged " ++ what ++ " inside TMU") 
            ]
          isMAddr = isAddr "address" mem tmuCacheSize priv

-- TODO: A significant technical issue is what to do with a user
-- program that tries to write into the TMU's memory. At the moment,
-- it can just do it, which is wrong.  But exactly how should it be
-- prevented?  At the abstract level, such a program is just ill
-- formed.  At the concrete level, we need to prevent it somehow.  And
-- the two behaviors need to correspond in an appropriate way.  How
-- does the storage allocator do this?

-- Check the cache and return a CS which has branched to the TMM if the current
-- instruction is not present there.  We rely on their being only one cache
-- line.
branchToTMMIfNeeded :: CS -> Maybe CS
branchToTMMIfNeeded cs@CS{..}
  | priv || cacheKey == instrKey = Nothing
  | otherwise                    =
    Just cs{ mem  = instrKey ++ drop tmuCacheKeySize mem
           , stk  = CRet (fmap (,False) pc) False : stk
           , pc   = Labeled H 0
           , priv = True }
  where
    instr = imem !! value pc
    
    cacheKey = take tmuCacheKeySize mem
    instrKey = map (Labeled H)
                   [instrToInt instr, lab1, lab2, lab3, labToInt . Just $ lab pc]
    
    stkLab = Just . cstkLab
    memLab = Just . lab
    
    f $~ (x,y,z) = case f (x,(y,z)) of (x',(y',z')) -> (x',y',z')
    infixr 0 $~
    
    -- There's a reason that the real thing doesn't use a stack machine!
    -- Getting at the arguments to check them is a bit of a pain.
    (lab1,lab2,lab3) = labToInt *** labToInt *** labToInt $~ case instr of
      Noop     -> (Nothing, Nothing, Nothing)
      Add      -> let a:b:_ = stk in (stkLab a, stkLab b, Nothing)
      Sub      -> let a:b:_ = stk in (stkLab a, stkLab b, Nothing)
      Push  n  -> (memLab n, Nothing, Nothing)
      Pop      -> (Nothing, Nothing, Nothing)
      Load     -> let a:_ = stk
                      b   = mem !! cstkValue a
                  in (stkLab a, memLab b, Nothing)
      Store    -> let a:b:_ = stk
                  in (stkLab a, stkLab b, memLab $ mem !! cstkValue a)
      Jump     -> let p:_ = stk in (stkLab p, Nothing, Nothing)
      JumpNZ   -> let a:p:_ = stk in (stkLab a, stkLab p, Nothing)
      Call a r -> (stkLab $ head stk, Nothing, Nothing)
      Return _ -> let Just retAddr = find (not . isCData) stk
                      retValLab | cstkReturns retAddr = stkLab $ head stk
                                | otherwise           = Nothing
                  in (retValLab, stkLab retAddr, Nothing)
      Halt     -> (Nothing, Nothing, Nothing)
      LabelOf  -> error "branchToTMMIfNeeded LabelOf not implemented"

-- TODO: Think about / discuss: What is the advantage of cutting
-- things up this way instead of weaving the definition of
-- well-formedness into the implementation of doStep (which would then
-- return an option)?  

stepFn :: CS -> CS
stepFn cs@CS{..} =
  case branchToTMMIfNeeded cs of 
    Just cs' -> cs'
    Nothing  -> doStep cs (labAt tmuCacheTagRes) (labAt tmuCacheTagResPC)
  where
    -- Ugh, fromJust.  The point is that we can promise that doStep will never
    -- refer to anything that was a Nothing.  This probably ought to get fixed (XXX).
    labAt addr | priv      = H
               | otherwise = fromJust . intToLab . value $ mem !! addr

-- Take a step in the concrete machine.  We don't do any label-checking,
-- assuming that we've been passed the correct labels.  (These labels will
-- always be read out of the TMM cache; see stepFn.)  Note that Call and Return
-- are special when the privileged bit is set: everything else forces labels to
-- H, but Call and Return instead *preserve* the label information that's
-- present on the call stack (so Call 0 0 with 10@L on the stack and the
-- privileged bit set will set the pc to 10@L, not 10@H).
doStep :: CS -> Label -> Label -> CS
doStep cs@CS{..} labRes labPC =
  let pc' = pc + 1 `withLab` labPC
  in case imem !! value pc of
       Noop ->
         cs{pc = pc'}
       Add ->
         let a:b:stk' = stk
         in cs{ stk = CData (Labeled labRes $ (((+) `on` cstkValue) a b)) : stk'
              , pc  = pc' }
       Sub ->
         let a:b:stk' = stk
         in cs{ stk = CData (Labeled labRes $ (((-) `on` cstkValue) a b)) : stk'
              , pc  = pc' }
       Push n ->
         cs{stk = CData n : stk, pc = pc'}
       Pop ->
         let _:stk' = stk
         in cs{stk = stk', pc = pc'}
       Load ->
         let a:stk' = stk
         in cs{stk = CData (mem !! cstkValue a `withLab` labRes) : stk', pc = pc'}
       Store ->
         let a:b:stk' = stk in
         cs{ mem = update (cstkValue a) (cstkData b `withLab` labRes) mem
           , stk = stk'
           , pc  = pc' }
       Jump ->
         let CData pcJ : stk' = stk in
         cs{stk = stk', pc = pcJ `withLab` labPC}
       JumpNZ ->
         let CData a : CData pcJ : stk' = stk in
         cs{stk = stk', pc = (if value a /= 0 then pcJ else pc') `withLab` labPC}
       Call a r ->
         let storePC = if priv then pc + 1 else (pc + 1) `withLab` labRes
             callPC  = (if priv then cstkData else Labeled labPC . cstkValue)
                     $ head stk in
         cs{ stk =  take a (drop 1 stk) -- TODO Should we labRes these?
                 ++ [CRet ((,r) <$> storePC) priv]
                 ++ drop (a+1) stk
           , pc  =  callPC }
       Return _ ->
         let (datas, CRet (Labeled rLab (retPC,r)) priv' : stk') =
               break (not . isCData) stk
         in cs { stk  =  map (CData . (`withLab` labRes) . cstkData)
                             (take (if r then 1 else 0) datas)
                      ++ stk'
               , pc   = Labeled (if priv then rLab else labPC) retPC
               , priv = priv'}
       Halt    -> error "doStep: Impossible: Can't execute Halt."
       LabelOf -> error "doStep: LabelOf not implemented"
