{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language PatternSynonyms #-}
{-# language LambdaCase #-}
{-# language KindSignatures #-}
{-# language GADTs #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}

module X86.Labeled
  ( Operand(..)
  , Instruction(..)
    -- * Helpers
  , apply1
  , apply2
  , apply3
  , apply4
  , apply5
  ) where

import X86.Unlabeled (Operator,OperandType(..),Scaled(..))
import X86.Label (Label)
import Topaz.Rec (Rec(RecCons,RecNil))
import Data.Kind (Type)

import Data.Int (Int8,Int16,Int32,Int64)

data Instruction = forall (opTys :: [OperandType]). Instruction
  (Operator opTys)
  (Rec Operand opTys)

data Operand :: OperandType -> Type where
  Reg_ ::
       !Word -- must be in range [0-15]
    -> Operand 'Reg
  Simd_ ::
       !Word -- must be in range [0-31]
    -> Operand 'Simd
  Mask_ ::
       !Word -- must be in range [0-7]
    -> Operand 'Mask
  Address64 :: -- ^ An address in which the full 64 bits are used from registers
       !Int32 -- displacement, technically some instructions allow 64-bit displacements, but this is not useful
    -> !Word -- base (register), technically should be optional
    -> !Scaled -- scaled offset added to base register
    -> Operand 'Mem
  RipRelative ::
       !Label
    -> Operand 'Mem
  Merging :: Operand 'Mode
  Zeroing :: Operand 'Mode
  Broadcasting :: Operand 'Broadcast
  Nonbroadcasting :: Operand 'Broadcast
  Imm8 :: !Int8 -> Operand ('Imm 8)
  Imm16 :: !Int16 -> Operand ('Imm 16)
  Imm32 :: !Int32 -> Operand ('Imm 32)
  Imm64 :: !Int64 -> Operand ('Imm 64)
  Rel8 :: -- ^ A label known to be near.
       !Label
    -> Operand ('Rel 8)
  Rel16 :: -- ^ A label known to be sort of near.
       !Label
    -> Operand ('Rel 16)
  Rel32 :: !Label -> Operand ('Rel 32)

deriving instance Eq (Operand ty)

apply1 :: Operator '[a] -> Operand a -> Instruction
apply1 op a = Instruction op
  $ RecCons a
  $ RecNil

apply2 :: Operator '[a,b] -> Operand a -> Operand b -> Instruction
apply2 op a b = Instruction op
  $ RecCons a
  $ RecCons b
  $ RecNil

apply3 :: Operator '[a,b,c] -> Operand a -> Operand b -> Operand c -> Instruction
apply3 op a b c = Instruction op
  $ RecCons a
  $ RecCons b
  $ RecCons c
  $ RecNil

apply4 :: Operator '[a,b,c,d] -> Operand a -> Operand b -> Operand c -> Operand d -> Instruction
apply4 op a b c d = Instruction op
  $ RecCons a
  $ RecCons b
  $ RecCons c
  $ RecCons d
  $ RecNil

apply5 :: Operator '[a,b,c,d,e] -> Operand a -> Operand b -> Operand c -> Operand d -> Operand e -> Instruction
apply5 op a b c d e = Instruction op
  $ RecCons a
  $ RecCons b
  $ RecCons c
  $ RecCons d
  $ RecCons e
  $ RecNil
