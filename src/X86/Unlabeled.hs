{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language PatternSynonyms #-}
{-# language LambdaCase #-}
{-# language KindSignatures #-}
{-# language GADTs #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}

module X86.Unlabeled
  ( OperandType(..)
  , Operator(..)
  , Scaled(..)
  ) where

import Data.Int (Int8,Int16,Int32,Int64)
import Topaz.Types (Rec(..))
import Data.Kind (Type)
import GHC.TypeNats (Nat)
import Data.Builder.Catenable.Text (Builder)
import Data.Builder.Catenable.Text (pattern (:<))
import Data.Text.Short (ShortText)
import Data.Void (Void)

import qualified Data.Builder.Catenable.Text as Builder
import qualified Data.Text.Short as TS
import qualified Topaz.Rec as Rec
import qualified Data.Void

-- TODO: Write down things that I learned about vpermilpd and m64bcst.
-- Notes:
-- * The instruction PADD is legacy. It is bad because it leaves the
--   upper bits of the destination register unmodified. VPADD is better.


data Operator :: [OperandType] -> Type where
  TestReg8Reg8 :: Operator '[ 'Reg, 'Reg ]
  TestReg16Reg16 :: Operator '[ 'Reg, 'Reg ]
  TestReg32Reg32 :: Operator '[ 'Reg, 'Reg ]
  TestReg64Reg64 :: Operator '[ 'Reg, 'Reg ]
  CmpReg8Reg8 :: Operator '[ 'Reg, 'Reg ]
  CmpReg16Reg16 :: Operator '[ 'Reg, 'Reg ]
  CmpReg32Reg32 :: Operator '[ 'Reg, 'Reg ]
  CmpReg64Reg64 :: Operator '[ 'Reg, 'Reg ]
  CmpReg64Imm8 :: Operator '[ 'Reg, 'Imm 8 ]
  CmpReg64Imm32 :: Operator '[ 'Reg, 'Imm 32 ]
  XorReg8Reg8 :: Operator '[ 'Reg, 'Reg ]
  XorReg16Reg16 :: Operator '[ 'Reg, 'Reg ]
  XorReg32Reg32 :: Operator '[ 'Reg, 'Reg ]
  XorReg64Reg64 :: Operator '[ 'Reg, 'Reg ]
  SetaReg8 :: Operator '[ 'Reg ]
  SetaeReg8 :: Operator '[ 'Reg ]
  SetbReg8 :: Operator '[ 'Reg ]
  SetbeReg8 :: Operator '[ 'Reg ]
  SetcReg8 :: Operator '[ 'Reg ]
  SeteReg8 :: Operator '[ 'Reg ]
  SetgReg8 :: Operator '[ 'Reg ]
  SetgeReg8 :: Operator '[ 'Reg ]
  SetlReg8 :: Operator '[ 'Reg ]
  SetleReg8 :: Operator '[ 'Reg ]
  SetnaReg8 :: Operator '[ 'Reg ]
  SetnaeReg8 :: Operator '[ 'Reg ]
  SetnbReg8 :: Operator '[ 'Reg ]
  SetnbeReg8 :: Operator '[ 'Reg ]
  SetncReg8 :: Operator '[ 'Reg ]
  SetneReg8 :: Operator '[ 'Reg ]
  SetngReg8 :: Operator '[ 'Reg ]
  SetngeReg8 :: Operator '[ 'Reg ]
  SetnlReg8 :: Operator '[ 'Reg ]
  SetnleReg8 :: Operator '[ 'Reg ]
  SetnoReg8 :: Operator '[ 'Reg ]
  SetnpReg8 :: Operator '[ 'Reg ]
  SetnsReg8 :: Operator '[ 'Reg ]
  SetnzReg8 :: Operator '[ 'Reg ]
  SetoReg8 :: Operator '[ 'Reg ]
  SetpReg8 :: Operator '[ 'Reg ]
  SetpeReg8 :: Operator '[ 'Reg ]
  SetpoReg8 :: Operator '[ 'Reg ]
  SetsReg8 :: Operator '[ 'Reg ]
  SetzReg8 :: Operator '[ 'Reg ]
  NegReg8 :: Operator '[ 'Reg ]
  NegReg16 :: Operator '[ 'Reg ]
  NegReg32 :: Operator '[ 'Reg ]
  NegReg64 :: Operator '[ 'Reg ]
  Lea16 :: Operator '[ 'Reg, 'Mem ]
  Lea32 :: Operator '[ 'Reg, 'Mem ]
  Lea64 :: Operator '[ 'Reg, 'Mem ]
  MovReg8Imm8 :: Operator '[ 'Reg, 'Imm 8 ]
  MovReg16Imm16 :: Operator '[ 'Reg, 'Imm 16 ]
  MovReg32Imm32 :: Operator '[ 'Reg, 'Imm 32 ]
  MovReg64Imm64 :: Operator '[ 'Reg, 'Imm 64 ]
  MovReg8Reg8 :: Operator '[ 'Reg, 'Reg ]
  MovReg16Reg16 :: Operator '[ 'Reg, 'Reg ]
  MovReg32Reg32 :: Operator '[ 'Reg, 'Reg ]
  MovReg64Reg64 :: Operator '[ 'Reg, 'Reg ]
  MovMem8Reg8 :: Operator '[ 'Mem, 'Reg ]
  MovMem16Reg16 :: Operator '[ 'Mem, 'Reg ]
  MovMem32Reg32 :: Operator '[ 'Mem, 'Reg ]
  MovMem64Reg64 :: Operator '[ 'Mem, 'Reg ]
  MovReg8Mem8 :: Operator '[ 'Reg, 'Mem ]
  MovReg16Mem16 :: Operator '[ 'Reg, 'Mem ]
  MovReg32Mem32 :: Operator '[ 'Reg, 'Mem ]
  MovReg64Mem64 :: Operator '[ 'Reg, 'Mem ]
  AddAcc8Imm8 :: Operator '[ 'Imm 8 ]
  AddAcc16Imm16 :: Operator '[ 'Imm 16 ]
  AddAcc32Imm32 :: Operator '[ 'Imm 32 ]
  AddAcc64Imm32 :: Operator '[ 'Imm 32 ]
  AddReg8Reg8 :: Operator '[ 'Reg, 'Reg ]
  AddReg16Reg16 :: Operator '[ 'Reg, 'Reg ]
  AddReg32Reg32 :: Operator '[ 'Reg, 'Reg ]
  AddReg64Reg64 :: Operator '[ 'Reg, 'Reg ]
  AddReg8Mem8 :: Operator '[ 'Reg, 'Mem ]
  AddReg16Mem16 :: Operator '[ 'Reg, 'Mem ]
  AddReg32Mem32 :: Operator '[ 'Reg, 'Mem ]
  AddReg64Mem64 :: Operator '[ 'Reg, 'Mem ]
  AddMem8Reg8 :: Operator '[ 'Mem, 'Reg ]
  AddMem16Reg16 :: Operator '[ 'Mem, 'Reg ]
  AddMem32Reg32 :: Operator '[ 'Mem, 'Reg ]
  AddMem64Reg64 :: Operator '[ 'Mem, 'Reg ]
  AddReg8Imm8 :: Operator '[ 'Reg, 'Imm 8]
  AddReg16Imm8 :: Operator '[ 'Reg, 'Imm 8]
  AddReg32Imm8 :: Operator '[ 'Reg, 'Imm 8]
  AddReg64Imm8 :: Operator '[ 'Reg, 'Imm 8]
  AddReg16Imm16 :: Operator '[ 'Reg, 'Imm 16]
  AddReg32Imm32 :: Operator '[ 'Reg, 'Imm 32]
  AddReg64Imm32 :: Operator '[ 'Reg, 'Imm 32]
  Vpadd64x8Reg :: Operator '[ 'Simd, 'Simd, 'Simd]
  Vpadd64x8Mem :: Operator '[ 'Simd, 'Simd, 'Mem]
  Vpadd32x8Reg :: Operator '[ 'Simd, 'Simd, 'Simd]
  Vpadd32x8Mem :: Operator '[ 'Simd, 'Simd, 'Mem]
  Vpadd16x8Reg :: Operator '[ 'Simd, 'Simd, 'Simd]
  Vpadd16x8Mem :: Operator '[ 'Simd, 'Simd, 'Mem]
  Vpadd32x16Reg :: Operator '[ 'Simd, 'Simd, 'Simd]
  Vpadd32x16Mem :: Operator '[ 'Simd, 'Simd, 'Mem]
  Vpadd16x16Reg :: Operator '[ 'Simd, 'Simd, 'Simd]
  Vpadd16x16Mem :: Operator '[ 'Simd, 'Simd, 'Mem]
  Vpadd8x16Reg :: Operator '[ 'Simd, 'Simd, 'Simd]
  Vpadd8x16Mem :: Operator '[ 'Simd, 'Simd, 'Mem]
  Vpadd16x32Reg :: Operator '[ 'Simd, 'Simd, 'Simd]
  Vpadd16x32Mem :: Operator '[ 'Simd, 'Simd, 'Mem]
  Vpadd8x32Reg :: Operator '[ 'Simd, 'Simd, 'Simd]
  Vpadd8x32Mem :: Operator '[ 'Simd, 'Simd, 'Mem]
  Vpadd4x32Reg :: Operator '[ 'Simd, 'Simd, 'Simd]
  Vpadd4x32Mem :: Operator '[ 'Simd, 'Simd, 'Mem]
  Vpadd8x64Reg :: Operator '[ 'Simd, 'Simd, 'Simd]
  Vpadd8x64Mem :: Operator '[ 'Simd, 'Simd, 'Mem]
  Vpadd4x64Reg :: Operator '[ 'Simd, 'Simd, 'Simd]
  Vpadd4x64Mem :: Operator '[ 'Simd, 'Simd, 'Mem]
  Vpadd2x64Reg :: Operator '[ 'Simd, 'Simd, 'Simd]
  Vpadd2x64Mem :: Operator '[ 'Simd, 'Simd, 'Mem]
  VpaddMask16x8Reg :: Operator '[ 'Simd, 'Mask, 'Mode, 'Simd, 'Simd]
  VpaddMask16x8Mem :: Operator '[ 'Simd, 'Mask, 'Mode, 'Simd, 'Mem]
  VpaddMask8x16Reg :: Operator '[ 'Simd, 'Mask, 'Mode, 'Simd, 'Simd]
  VpaddMask8x16Mem :: Operator '[ 'Simd, 'Mask, 'Mode, 'Simd, 'Mem]
  VpaddMask4x32Reg :: Operator '[ 'Simd, 'Mask, 'Mode, 'Simd, 'Simd]
  VpaddMask4x32Mem :: Operator '[ 'Simd, 'Mask, 'Mode, 'Simd, 'Mem, 'Broadcast]
  VpaddMask2x64Reg :: Operator '[ 'Simd, 'Mask, 'Mode, 'Simd, 'Simd]
  VpaddMask2x64Mem :: Operator '[ 'Simd, 'Mask, 'Mode, 'Simd, 'Mem, 'Broadcast]
  Jo :: Operator '[ 'Rel n ]
  Jno :: Operator '[ 'Rel n ]
  Js :: Operator '[ 'Rel n ]
  Jns :: Operator '[ 'Rel n ]
  Jz :: Operator '[ 'Rel n ]
  Jnz :: Operator '[ 'Rel n ]
  Jc :: Operator '[ 'Rel n ]
  Jnc :: Operator '[ 'Rel n ]
  Jle :: Operator '[ 'Rel n ]

-- Only used promoted.
data OperandType
  = Reg -- general purpose register
  | Simd -- simd register
  | Mask -- mask register
  | Mode -- merging or zeroing
  | Broadcast -- broadcast or no broadcast
  | Mem
  | Offset
  | Rel Nat
  | Imm Nat

data Scaled
  = ScaledSome
      !Word -- register
      !Word -- multiplier
  | ScaledNone
  deriving (Show,Eq)
