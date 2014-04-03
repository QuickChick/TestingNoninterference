{-
   Todo:

     - Another good refinement would be to add a conditional branch
       instruction and make the allocator return -1 if there's not
       enough memory left.

     - We should change the abstract wf predicate so that the abstract
       machine doesn't try to use memory that isn't allocated.  We
       might even want to change the memory representation of the
       abstract machine so that unallocated addresses are not even
       represented.  

     - Add external behaviors.
-}

{-# LANGUAGE MultiParamTypeClasses, RecordWildCards #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies #-} -- For the TH magic
module SinglecoreAlloc where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List
import Test.QuickCheck.Gen
import Test.QuickCheck
import Text.PrettyPrint hiding (int)

import Aggregate
import Machine
import Pretty
import Util
import CollectTests

data Instr =
    Store 
  | Load
  | Push Int
  | Swap Int
  | Add
  | Jump
  | Call Int
  | Noop
  deriving (Show, Eq, Read)

-- the size of the memory reserved for the allocator
minaddr, maxaddr, maxiaddr :: Int

minaddr = 1

maxaddr = 6
maxiaddr = 20

iaddr :: Gen Int
iaddr = choose (0, maxiaddr)

-- Like `elements`, but with a meaningful error message
elements' :: String -> [a] -> Gen a
elements' tag [] = error $ "elements " ++ tag
elements' _ xs = elements xs

-- Like `(!!)`, but with a meaningful error message
ix :: String -> [a] -> Int -> a
ix s xs n | length xs <= n = error s
ix _ xs n = xs !! n

ainstr :: Int -> AS -> Gen Instr
ainstr imem as@(AS{amem=mem, astk=stk}) =
  frequency $
    [ (40, return Jump) | length stk >= 1, validIAddr (head stk) ] ++
    [ (2, Call <$> choose (ioffs, ioffs + imem - 1)) ] ++
    -- If we're generating the last instruction, don't bother
    -- generating anything except BI and Call.
    if length (aimem as) + 1 == imem then [] else
    [ (60, return Store) | length stk >= 2, (head stk - minaddr) `isIndex` mem ] ++
    [ (60, return Load)  | length stk >= 1, (head stk - minaddr) `isIndex` mem ] ++
    [ (5, Push <$> int) ] ++
    [ (40, return Add) | length stk >= 2 ] ++
    [ (50, return $ Call 0) | length stk >= 2, possibleAllocations as /= [] ] ++
    [ (1, return Noop) ]
  where
    ioffs = length alloc
    validIAddr p = p >= ioffs && p < imem + ioffs

ainstrs :: Int -> AS -> Gen [Instr]
ainstrs imem as@(AS{amem = mem, astk = stk}) =
  frequency $
    [ (10, (:[]) <$> ainstr imem as) ] ++
    if length (aimem as) + 2 >= imem then [] else
    [ (1, pushAndDo Jump <$> choose (ioffs, imem + ioffs - 1)) ] ++
    [ (1, pushAndDo Load <$> choose (minaddr, minaddr + length mem - 1)) | not $ null mem ] ++
    [ (2, pushAndDo Store <$> choose (minaddr, minaddr + length mem - 1)) | not $ null mem, length stk >= 1 ]
  where
    ioffs = length alloc
    pushAndDo i a = [Push a, i]

instance Arbitrary Instr where
  arbitrary = oneof [return Store, return Load,
                     fmap Push int, return Add, 
                     return Jump, fmap Call iaddr, 
                     return Noop]
  shrink Noop = []
  shrink i = 
    Noop :      -- Easiest way to shrink an instruction is replacing it with a Noop.
    case i of   -- Otherwise...
      Store -> []                    -- Store and Load can't be shrunk
      Load -> []
      Jump -> []
      Push x -> map Push $ shrink x    
      Add -> []
      Call a -> map Call $ shrink a  -- Probably not.
      _ -> []

data CS = CS {mem::[Int], 
              imem::[Instr], 
              stk::[Int], 
              pc::Int, 
              counter::Int}    -- just to provide some nondeterminism
                               -- at the concrete level - not used by
                               -- the ISA at the moment
  deriving (Eq, Read)

instance Pretty CS where
  pretty CS{..} =
    text "CS" <+> record
      [ field "mem" $ l mem
      , field "imem" $ l imem
      , field "stk" $ l stk
      , field "pc" $ text $ show pc
      , field "counter" $ text $ show counter
      ]
    where
      l :: Show a => [a] -> Doc
      l xs = list $ map (text . show) xs
      field s d = sep [ text s <+> text "=", nest 2 d ]

instance Show CS where
  show = show . pretty

instance Arbitrary CS where
  arbitrary = do
    mem <- listOf int
    imem <- arbitrary
    stk <- listOf int
    return CS {mem=mem, imem=imem, stk=stk, pc=0, counter=0}
  shrink (CS a b c d e) = 
    [ CS a b c d e | (a,b,c,d,e) <- shrink (a,b,c,d,e) ]

store :: Int -> Int -> CS -> CS
store a b cs = 
  cs { mem = update a b (mem cs) }

bumpCounter :: CS -> Gen CS
bumpCounter cs = do
  n <- choose (1,10)
  return $ cs { counter = counter cs + n }

instance Machine CS where
  isStep cs cs' = 
    let cs'' = stepFn cs in
        isWF cs
      && cs' == cs'' { counter = counter cs' }
      && counter cs' > counter cs

  step = defaultStep "SinglecoreAlloc.CS" $ bumpCounter . stepFn

  wf cs 
    = wfChecks $
      [ (pc cs `isIndex` imem cs)        `orElse` "PC out of range"
      , (instr_check (imem cs !! pc cs)) `orElse` "Invalid instruction"
      ]
    where instr_check Store 
            = length (stk cs) >= 2
               && head (stk cs) `isIndex` mem cs
          instr_check Load 
            = length (stk cs) >= 1 &&
                 head (stk cs) `isIndex` mem cs
          instr_check Add 
            =  length (stk cs) >= 2
          instr_check Jump 
            = length (stk cs) >= 1
          instr_check (Swap n) 
            = length (stk cs) >= n + 2
          instr_check _ = True

stepFn :: CS -> CS
stepFn cs = 
  case imem cs !! pc cs of
    Store -> 
      let a:b:stk' = stk cs in
      store a b cs {stk = stk', pc = pc cs + 1}
    Push n -> 
      cs {stk = n : stk cs, pc = pc cs + 1}
    Load -> 
      let a:stk' = stk cs in
      cs {stk = (mem cs !! a) : stk', pc = pc cs + 1}
    Add -> 
      let a:b:stk' = stk cs in
      cs {stk = (a+b) : stk', pc = pc cs + 1}
    Swap n ->
      let (a:as, b:bs) = splitAt (n + 1) (stk cs) in
      cs {stk = b:as ++ a:bs, pc = pc cs + 1}
    Jump ->
      let p:stk' = stk cs in
      cs { stk = stk', pc = p }
    Call a -> 
      cs {stk = (pc cs + 1) : stk cs, pc = a}
    Noop -> 
      cs {pc = pc cs + 1}

-----------------------------------------------------------
-- Abstract 

data AS = AS {amem::[Int],       -- starting at minaddr
              aimem::[Instr],    -- starting after alloc routine
              astk::[Int],       -- same interpretation as in CS
              apc::Int,          -- ditto
              acounter::Int,     -- ditto
              allocated::[Int]}  -- which memory cells have been allocated
  deriving (Eq, Read)

instance Show AS where
  show = show . pretty

instance Pretty AS where
  pretty AS{..} =
    text "AS" <+> record
      [ field "amem" $ l amem
      , field "aimem" $ l aimem
      , field "astk" $ l astk
      , field "apc" $ text $ show apc
      , field "acounter" $ text $ show acounter
      , field "allocated" $ l allocated
      ]
    where
      l :: Show a => [a] -> Doc
      l xs = list $ map (text . show) xs
      field s d = sep [ text s <+> text "=", nest 2 d ]

instance Arbitrary AS where
  arbitrary = do
    -- Choose how many instructions to generate
    n <- choose (20, 50)  
    as <- initAs n
    is <- genIs n as
    return $ as { aimem = is }
    where
      initAs n = do
        let addr = choose (length alloc, length alloc + n - 1)
        mem <- listOf $ frequency [(1, int), (2, addr)]
        stk <- listOf $ frequency [(1, int), (4, choose (minaddr, length mem + minaddr)), (1, addr)]
        return AS {amem=mem,           -- random memory contents
                   aimem=[],           -- instructions are generated later
                   astk=stk,           -- random initial stack
                   apc=length alloc,   -- initial PC is the first instr after the alloc routine
                   acounter=0,         -- initial counter is always 0
                   allocated = []}     -- nothing is allocated initially

      genIs n as = genIs' False [] n n as []

      -- The tbl maps pc's to states stored when encountering forward jumps.
      -- The tainted flag records if the current state is accurate. Important:
      -- ignore forward jumps in tainted states.
      genIs' _ _ _ 0 as _ = return $ aimem as
      genIs' tainted tbl n m as0 is = do
        let pc = length alloc + length (aimem as0)
            stored = lookup pc tbl
            fromFwdJump = isJust stored && tainted
            as | not tainted = as0
               | otherwise   = (fromMaybe as0 stored) { aimem = aimem as0 }
        (i, is) <- case is of
          i:is | not fromFwdJump -> return (i, is)
          _ -> do
            i:is <- ainstrs n as
            return (i, is)
        let as' = as { aimem = aimem as0 ++ [i],
                       -- Hack: pretend that the next instr is the one we generate
                       -- even if it isn't
                       apc = pc }
        () <- unless (isWF as') $ error $ "bad as: " ++ show as' ++ "\nis = " ++ show is
        as'' <- step as'
        let jump = case i of
              Call p | p /= 0 -> Just p
              Jump -> Just $ head (astk as)
              _      -> Nothing

            tainted' = tainted && isNothing stored

            tbl' | tainted' = tbl
                 | otherwise = case jump of
                    Just p | p > pc -> (p, as'') : tbl
                    _               -> tbl

            tainted'' = maybe False (/= pc + 1) jump || tainted'
        genIs' tainted'' tbl' n (m - 1) as'' is

  shrink (AS a b c d e f) = 
    -- note that we don't change the pc (d) when we are shrinking test
    -- cases.  (This way of writing the code is an ugly hack -- we
    -- should write it out more explicitly.)
    [ AS a b c d e f | (a,b,c,e,f) <- shrink (a,b,c,e,f) ]

instance Layer CS AS where
  concretize as =
    CS {
      mem = (maximum ((minaddr - 1) : allocated as) + 1) : replicate (minaddr - 1) (-1)
            ++ amem as,
         -- We initially put -1 in the return address temp so that, if
         -- we happen to branch through it before we call the allocator
         -- the first time, we don't fall into a loop (this is not
         -- completely satisfactory -- would be better to make
         -- concretize into a generator, not a function)
      imem = alloc ++ aimem as,
         -- We place the allocation routine at address 0
      stk = astk as,
      pc = apc as,
      counter = acounter as
    }

  abstract cs =
    AS {
      amem = drop minaddr (mem cs),
      aimem = drop (length alloc) (imem cs),
      astk = stk cs,
      apc = pc cs,
      acounter = counter cs,
      allocated = [minaddr .. (mem cs !! hp) - 1]
          -- Mark all locations up to the hp as allocated.  (Is this
          -- what we really want to say?  The argument for "yes" is
          -- that we are writing this abstraction function in light of
          -- a particular implementation of the allocator.)
    }

  abstractable cs = pc cs >= length alloc

performAlloc :: Int -> AS -> AS
performAlloc ptr as =
  let a:b:stk' = astk as in
  as {amem = update (ptr - minaddr) a $ 
             update (ptr - minaddr + 1) b $ 
             amem as,
      astk = ptr : stk',
      apc = apc as + 1,
      allocated = allocated as ++ [ptr,ptr+1]}

instance Machine AS where
  isStep as as' = 
    case aimem as !! (apc as - length alloc) of
      Call 0 -> 
        astk as' /= [] &&
        let a:b:stk' = astk as 
            ptr:_ = astk as'
        in allocated as \\ [ptr,ptr+1] == allocated as &&
           as' == (performAlloc ptr as) { acounter = acounter as' }
      _ -> isStep (concretize as) (concretize as')
             -- This is a bit of a hack, to save writing the abstract
             -- isStep function explicitly (which would duplicate a
             -- lot of code).  Morally, we shouldn't be using
             -- concretize here, but we believe it's OK anyway because
             -- this is one of the instructions that *doesn't* involve
             -- allocation, so it should not touch any of the
             -- allocator data structures (which are the ones for
             -- which the concretize function makes up bogus initial
             -- values).

  step as = case aimem as !! (apc as - length alloc) of
    Call 0 -> do
      ptr <- -- Now only used to improve generation, so good
             -- if it's precise. When used to test layers further
             -- up it should be generated from possibleAllocations:
             -- elements $ possibleAllocations as
             return (maximum (minaddr - 1 : allocated as) + 1)
      return $ performAlloc ptr as
    _ -> abstract <$> step (concretize as)

  wf as =
    wfChecks $
       [ wf (concretize as) -- This wouldn't work if we made concretize a generator
       , (apc as >= length alloc)
             `orElse` "Must be executing in user-space"
       , instr_check (aimem as !! pc) ]
    where instr_check Store
            = (head (astk as) >= minaddr)
                 `orElse` "Illegal to write to hp from abstract program"
          instr_check Load
            = (head (astk as) >= minaddr)
                 `orElse` "Illegal load"
          instr_check Jump
            = (head (astk as) >= length alloc)
                  `orElse` "Illegal to jump into the allocator"
          instr_check (Call 0)
            = (length (astk as) >= 2 &&
                   possibleAllocations as /= [])
                  `orElse` "Invalid Call-0"
          instr_check (Call a)
            = (a >= length alloc)
                  `orElse` "Illegal to call into the allocator"
          instr_check _ = WF
          pc = apc as - length alloc

possibleAllocations :: AS -> [Int]
possibleAllocations as =
  [ ptr | ptr <- [minaddr..length (amem as) + minaddr - 2], all (`notElem` allocated as) [ptr, ptr + 1] ]

-----------------------------------------------------------

instructions :: [CS] -> [String]
instructions css =
  [ opCode $ imem cs !! pc cs | cs <- init css ]

opCode :: Instr -> String
opCode (Call 0) = "Alloc"
opCode i = takeWhile (/= ' ') $ show i

lshow :: Show a => [a] -> String
lshow = concat <$>  sequence [show . length, pure "@", show]

prop_correct :: Property
prop_correct = gen_prop_correct (length alloc * 10) 200 arbitrary $ \css ass prop ->
  let pcs = [ apc as | as <- ass ]
      is  = [ aimem as !! (apc as - length alloc) | as <- ass ]
      pccount = sort $ map snd $ bagify pcs
  in
  whenFail (do
    mapM_ print ass
    putStrLn "---"
    mapM_ print css
  ) $
  collect ((5 *) $ (`div` 5) $ length $ nub pcs) $
  -- collect (maximum $ 0 : map snd (bagify pcs)) $
  -- collect (length ass) $
  -- collect pccount $
  -- collect (bagify pcs) $
  aggregate [ opCode (ix ("< " ++ lshow (aimem as) ++ " !! (" ++ show (apc as) ++ " - " ++ lshow alloc ++ " == " ++ show (apc as - length alloc) ++ ") >") (aimem as) (apc as - length alloc)) | as <- ass ] $
  prop -- && and [ take 1 (astk as) < [100] | as <- take 1 $ reverse ass ]
  -- && (any (/=1) pccount || length pccount > 2 || (pc (css !! length ass) == length alloc))

---------------------------------------------------------

cs1 :: CS
cs1 = CS {
  mem = [0,0,0],
  imem = [Store],
  stk = [1,2],
  pc = 0,
  counter = 0
  }

hp, ret :: Int
hp = 0
ret = 1

alloc :: [Instr]
alloc =     -- expect two args (the CAR and CDR of a new cell) on the stack
              -- ret:a:b:_
  [ Swap 1    -- b:a:ret:_
  , Push hp   -- 0:b:a:ret:_      store the CAR into the first free cell (hp)
  , Load      -- hp:b:a:ret:_
  , Push 1    -- 1:hp:b:a:ret:_
  , Add       -- 1+hp:b:a:ret:_
  , Store     -- a:ret:_          *(hp + 1) = b
  , Push hp   -- 0:a:ret:_        store the CDR into the next free cell (hp+1)
  , Load      -- hp:a:ret:_
  , Store     -- ret:_            *hp = a
  , Push hp   -- 0:ret:_          save a copy of current heap pointer on the stack
  , Load      -- hp:ret:_
  , Push hp   -- 0:hp:ret:_       bump hp by 2
  , Load      -- hp:hp:ret:_
  , Push 2    -- 2:hp:hp:ret:_
  , Add       -- hp+2:hp:ret:_
  , Push hp   -- 0:hp+2:hp:ret:_
  , Store     -- hp:ret:_         *0 = hp + 2
  , Swap 0    -- ret:hp:_
  , Jump ]    -- hp:_             return (leaving old hp on the stack)

cs2 :: CS
cs2 = CS {
  mem = [2,0,0,0],
  imem = alloc,
  stk = [-1,1,2],
  pc = 0,
  counter = 0
  }

singlecoreAllocProps :: [(Name,[(Type,Property)])]
singlecoreAllocProps = $(moduleProps [t|()|] [])

main :: IO ()
main = defaultTestingMain singlecoreAllocProps
