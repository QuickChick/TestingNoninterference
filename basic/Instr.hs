{-# LANGUAGE ImplicitParams, FlexibleContexts, UndecidableInstances #-}

module Instr where

import Test.QuickCheck.Gen
import Test.QuickCheck
import Control.Monad
import Control.Applicative

import Labels
import Flags
import Observable

import LaTeX
import Util

{-------------------------------- Instructions --------------------------------}

-- The data in our machines
type Atom = Labeled Int

-- A very simple stack machine.  Unless otherwise indicated, instructions do not
-- alter memory and increment the PC by one.  No instruction alters the
-- instruction memory, and only Noop doesn't change the stack.  The stack has
-- two kinds of entries: data entries and return address entries.  No
-- instructions other than call and return can cross return address entries;
-- they are much the same as the bottom of a stack.  We notate return addresses
-- as pairs (retPC,retArgs); except as explicitly noted, stacks in the
-- documentation don't include return addresses, and notionally end right above
-- them.
data Instr =
  -- No-op --
    Noop
  
  -- Pure stack instructions --
  | Add
    -- Stack:  a b _ -> (a+b) _
  | Sub
    -- Stack:  a b _ -> (a-b) _
  | Push Atom -- a :: Atom
    -- Stack:  _ -> a _
  | Pop
    -- Stack:  a _ -> _
  
  -- Memory instructions --
  | Load
    -- Stack:  addr _ -> mem[addr] _
  | Store
    -- Stack:  addr val _ -> _
    -- Memory: mem[addr] := val
  
  -- Basic control flow --
  | Jump
    -- Stack:  iaddr _ -> _
    -- PC:     pc := iaddr
  | JumpNZ
    -- Stack:  a iaddr _ -> _
    -- PC:     pc := if a /= 0 then iaddr else pc + 1
  
  -- Function calls --
  | Call Int Bool -- number of args (A) and whether the function returns a value (R)
    -- Stack:  iaddr arg1 ... argA _ -> arg1 ... argA (pc,R)
    -- PC:     pc := iaddr
  | Return Bool -- Bool argument only so that we can introduce a bug
    -- Stack:  ret ...junk... (iaddr,True)  _ -> ret _
    --             ...junk... (iaddr,False) _ -> _
    -- PC:     pc := iaddr
  
  -- Halting
  | Halt
  
  -- Label introspection (bug for this machine)
  | LabelOf
  deriving (Show, Eq, Read)

instance LaTeX Instr where
  toLaTeX (Push   a)   = "\\ii{Push}\\;" ++ toLaTeX a
  toLaTeX (Call   n r) = "\\ii{Call}\\CallArgs{" ++ toLaTeX n ++ "}\\CallRet{" ++ toLaTeX r ++ "}"
  toLaTeX (Return r)   = "\\ii{Return}\\ReturnRet{" ++ toLaTeX r ++ "}"
  toLaTeX i            = "\\ii{" ++ show i ++ "}"

-- CH: this is usually called Instruction Opcode!
data InstrKind =
    NOOP
  | ADD
  | SUB
  | PUSH
  | POP
  | LOAD
  | STORE
  | JUMP
  | JUMPNZ
  | CALL
  | RETURN
  | HALT
  | LABELOF
  deriving (Show, Eq, Read)

instance Flaggy DynFlags => Arbitrary Instr where
  arbitrary = oneof $ [Push <$> labeled int,
                       return Pop,
                       return Add,
                       return Load,
                       return Store,
                       return Noop] ++
                      [return Jump | jumpAllowed gi] ++
                      (if callsAllowed gi then 
                         [liftM2 Call (choose (0,conf_max_call_args getFlags))
                                      arbitrary,
                          liftM Return arbitrary]
                       else []) ++
                      [return Halt]
    where gi :: Flaggy DynFlags => GenInstrs
          gi = gen_instrs getFlags


  shrink Noop = []
  shrink i = 
    Noop :      -- Easiest way to shrink an instruction is replacing it with a Noop.
    case i of   -- Otherwise...
      Push x   -> map Push $ shrink x    
      JumpNZ   -> [Jump]
      Call a r -> Jump : map (uncurry Call) (shrink (a,r))
      Return True -> [Return False]
      _        -> []

instance Flaggy DynFlags => Observable Instr where
  Push a ~~~ Push a' = a ~~~ a'
  i ~~~ i' = i == i'

  vary _ = error "Observable Instr implements no vary"
{- dead code:
  vary (Push a) = liftM Push (vary a)
  vary i = return i
-}

  shrinkV (Variation Noop Noop) = []
  shrinkV (Variation i i') = 
    (if shrink_to_noop getFlags then (Variation Noop Noop :) else id) $
    case (i,i') of 
      (Push a,   Push a')    -> map (fmap Push) $ shrinkV (Variation a a')
      (JumpNZ,   JumpNZ)     -> [Variation Jump Jump]
      (Call a r, Call a' r') -> 
         Variation Jump Jump :
         map (fmap (uncurry Call)) (shrinkV (Variation (a,r) (a',r')))
      (Return True, Return True) ->
         [Variation (Return False) (Return False)]
      _                      -> []
