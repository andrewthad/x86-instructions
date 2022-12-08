{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language PatternSynonyms #-}
{-# language LambdaCase #-}
{-# language KindSignatures #-}
{-# language GADTs #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}

module X86.Operator
  ( encode
  , elementWidth
  , elementCount
  ) where

import Data.Text.Short (ShortText)
import X86.Unlabeled (Operator(..))

encode :: Operator args -> ShortText
encode = \case
  TestReg8Reg8 -> "TEST"
  TestReg16Reg16 -> "TEST"
  TestReg32Reg32 -> "TEST"
  TestReg64Reg64 -> "TEST"
  CmpReg8Reg8 -> "CMP"
  CmpReg16Reg16 -> "CMP"
  CmpReg32Reg32 -> "CMP"
  CmpReg64Reg64 -> "CMP"
  CmpReg64Imm8 -> "CMP"
  CmpReg64Imm32 -> "CMP"
  XorReg8Reg8 -> "XOR"
  XorReg16Reg16 -> "XOR"
  XorReg32Reg32 -> "XOR"
  XorReg64Reg64 -> "XOR"
  SetaReg8 -> "SETA"
  SetaeReg8 -> "SETAE"
  SetbReg8 -> "SETB"
  SetbeReg8 -> "SETBE"
  SetcReg8 -> "SETC"
  SeteReg8 -> "SETE"
  SetgReg8 -> "SETG"
  SetgeReg8 -> "SETGE"
  SetlReg8 -> "SETL"
  SetleReg8 -> "SETLE"
  SetnaReg8 -> "SETNA"
  SetnaeReg8 -> "SETNAE"
  SetnbReg8 -> "SETNB"
  SetnbeReg8 -> "SETNBE"
  SetncReg8 -> "SETNC"
  SetneReg8 -> "SETNE"
  SetngReg8 -> "SETNG"
  SetngeReg8 -> "SETNGE"
  SetnlReg8 -> "SETNL"
  SetnleReg8 -> "SETNLE"
  SetnoReg8 -> "SETNO"
  SetnpReg8 -> "SETNP"
  SetnsReg8 -> "SETNS"
  SetnzReg8 -> "SETNZ"
  SetoReg8 -> "SETO"
  SetpReg8 -> "SETP"
  SetpeReg8 -> "SETPE"
  SetpoReg8 -> "SETPO"
  SetsReg8 -> "SETS"
  SetzReg8 -> "SETZ"
  NegReg8 -> "NEG"
  NegReg16 -> "NEG"
  NegReg32 -> "NEG"
  NegReg64 -> "NEG"
  Lea16 -> "LEA"
  Lea32 -> "LEA"
  Lea64 -> "LEA"
  MovReg8Imm8 -> "MOV"
  MovReg16Imm16 -> "MOV"
  MovReg32Imm32 -> "MOV"
  MovReg64Imm64 -> "MOV"
  MovReg8Reg8 -> "MOV"
  MovReg16Reg16 -> "MOV"
  MovReg32Reg32 -> "MOV"
  MovReg64Reg64 -> "MOV"
  MovMem8Reg8 -> "MOV"
  MovMem16Reg16 -> "MOV"
  MovMem32Reg32 -> "MOV"
  MovMem64Reg64 -> "MOV"
  MovReg8Mem8 -> "MOV"
  MovReg16Mem16 -> "MOV"
  MovReg32Mem32 -> "MOV"
  MovReg64Mem64 -> "MOV"
  AddAcc8Imm8 -> "ADD"
  AddAcc16Imm16 -> "ADD"
  AddAcc32Imm32 -> "ADD"
  AddAcc64Imm32 -> "ADD"
  AddReg8Reg8 -> "ADD"
  AddReg16Reg16 -> "ADD"
  AddReg32Reg32 -> "ADD"
  AddReg64Reg64 -> "ADD"
  AddReg8Mem8 -> "ADD"
  AddReg16Mem16 -> "ADD"
  AddReg32Mem32 -> "ADD"
  AddReg64Mem64 -> "ADD"
  AddMem8Reg8 -> "ADD"
  AddMem16Reg16 -> "ADD"
  AddMem32Reg32 -> "ADD"
  AddMem64Reg64 -> "ADD"
  AddReg8Imm8 -> "ADD"
  AddReg16Imm8 -> "ADD"
  AddReg32Imm8 -> "ADD"
  AddReg64Imm8 -> "ADD"
  AddReg16Imm16 -> "ADD"
  AddReg32Imm32 -> "ADD"
  AddReg64Imm32 -> "ADD"
  Vpadd64x8Reg -> "VPADDB"
  Vpadd64x8Mem -> "VPADDB"
  Vpadd32x8Reg -> "VPADDB"
  Vpadd32x8Mem -> "VPADDB"
  Vpadd16x8Reg -> "VPADDB"
  Vpadd16x8Mem -> "VPADDB"
  Vpadd32x16Reg -> "VPADDW"
  Vpadd32x16Mem -> "VPADDW"
  Vpadd16x16Reg -> "VPADDW"
  Vpadd16x16Mem -> "VPADDW"
  Vpadd8x16Reg -> "VPADDW"
  Vpadd8x16Mem -> "VPADDW"
  Vpadd16x32Reg -> "VPADDD"
  Vpadd16x32Mem -> "VPADDD"
  Vpadd8x32Reg -> "VPADDD"
  Vpadd8x32Mem -> "VPADDD"
  Vpadd4x32Reg -> "VPADDD"
  Vpadd4x32Mem -> "VPADDD"
  Vpadd8x64Reg -> "VPADDQ"
  Vpadd8x64Mem -> "VPADDQ"
  Vpadd4x64Reg -> "VPADDQ"
  Vpadd4x64Mem -> "VPADDQ"
  Vpadd2x64Reg -> "VPADDQ"
  Vpadd2x64Mem -> "VPADDQ"
  VpaddMask16x8Reg -> "VPADDB"
  VpaddMask16x8Mem -> "VPADDB"
  VpaddMask8x16Reg -> "VPADDW"
  VpaddMask8x16Mem -> "VPADDW"
  VpaddMask4x32Reg -> "VPADDD"
  VpaddMask4x32Mem -> "VPADDD"
  VpaddMask2x64Reg -> "VPADDQ"
  VpaddMask2x64Mem -> "VPADDQ"
  Jo -> "JO"
  Jno -> "JNO"
  Js -> "JS"
  Jns -> "JNS"
  Jz -> "JZ"
  Jnz -> "JNZ"
  Jc -> "JC"
  Jnc -> "JNC"
  Jle -> "JLE"



elementWidth :: Operator args -> Word
elementWidth = \case
  TestReg8Reg8 -> 0
  TestReg16Reg16 -> 1
  TestReg32Reg32 -> 2
  TestReg64Reg64 -> 3
  CmpReg8Reg8 -> 0
  CmpReg16Reg16 -> 1
  CmpReg32Reg32 -> 2
  CmpReg64Reg64 -> 3
  CmpReg64Imm8 -> 3
  CmpReg64Imm32 -> 3
  XorReg8Reg8 -> 0
  XorReg16Reg16 -> 1
  XorReg32Reg32 -> 2
  XorReg64Reg64 -> 3
  SetaReg8 -> 0
  SetaeReg8 -> 0
  SetbReg8 -> 0
  SetbeReg8 -> 0
  SetcReg8 -> 0
  SeteReg8 -> 0
  SetgReg8 -> 0
  SetgeReg8 -> 0
  SetlReg8 -> 0
  SetleReg8 -> 0
  SetnaReg8 -> 0
  SetnaeReg8 -> 0
  SetnbReg8 -> 0
  SetnbeReg8 -> 0
  SetncReg8 -> 0
  SetneReg8 -> 0
  SetngReg8 -> 0
  SetngeReg8 -> 0
  SetnlReg8 -> 0
  SetnleReg8 -> 0
  SetnoReg8 -> 0
  SetnpReg8 -> 0
  SetnsReg8 -> 0
  SetnzReg8 -> 0
  SetoReg8 -> 0
  SetpReg8 -> 0
  SetpeReg8 -> 0
  SetpoReg8 -> 0
  SetsReg8 -> 0
  SetzReg8 -> 0
  NegReg8 -> 0
  NegReg16 -> 1
  NegReg32 -> 2
  NegReg64 -> 3
  Lea16 -> 1
  Lea32 -> 2
  Lea64 -> 3
  MovReg8Imm8 -> 0
  MovReg16Imm16 -> 1
  MovReg32Imm32 -> 2
  MovReg64Imm64 -> 3
  MovReg8Reg8 -> 0
  MovReg16Reg16 -> 1
  MovReg32Reg32 -> 2
  MovReg64Reg64 -> 3
  MovMem8Reg8 -> 0
  MovMem16Reg16 -> 1
  MovMem32Reg32 -> 2
  MovMem64Reg64 -> 3
  MovReg8Mem8 -> 0
  MovReg16Mem16 -> 1
  MovReg32Mem32 -> 2
  MovReg64Mem64 -> 3
  AddAcc8Imm8 -> 0
  AddAcc16Imm16 -> 1
  AddAcc32Imm32 -> 2
  AddAcc64Imm32 -> 3
  AddReg8Reg8 -> 0
  AddReg16Reg16 -> 1
  AddReg32Reg32 -> 2
  AddReg64Reg64 -> 3
  AddReg8Mem8 -> 0
  AddReg16Mem16 -> 1
  AddReg32Mem32 -> 2
  AddReg64Mem64 -> 3
  AddMem8Reg8 -> 0
  AddMem16Reg16 -> 1
  AddMem32Reg32 -> 2
  AddMem64Reg64 -> 3
  AddReg8Imm8 -> 0
  AddReg16Imm8 -> 1
  AddReg32Imm8 -> 2
  AddReg64Imm8 -> 3
  AddReg16Imm16 -> 1
  AddReg32Imm32 -> 2
  AddReg64Imm32 -> 3
  Vpadd64x8Reg -> 0
  Vpadd64x8Mem -> 0
  Vpadd32x8Reg -> 0
  Vpadd32x8Mem -> 0
  Vpadd16x8Reg -> 0
  Vpadd16x8Mem -> 0
  Vpadd32x16Reg -> 1
  Vpadd32x16Mem -> 1
  Vpadd16x16Reg -> 1
  Vpadd16x16Mem -> 1
  Vpadd8x16Reg -> 1
  Vpadd8x16Mem -> 1
  Vpadd16x32Reg -> 2
  Vpadd16x32Mem -> 2
  Vpadd8x32Reg -> 2
  Vpadd8x32Mem -> 2
  Vpadd4x32Reg -> 2
  Vpadd4x32Mem -> 2
  Vpadd8x64Reg -> 3
  Vpadd8x64Mem -> 3
  Vpadd4x64Reg -> 3
  Vpadd4x64Mem -> 3
  Vpadd2x64Reg -> 3
  Vpadd2x64Mem -> 3
  VpaddMask16x8Reg -> 0
  VpaddMask16x8Mem -> 0
  VpaddMask8x16Reg -> 1
  VpaddMask8x16Mem -> 1
  VpaddMask4x32Reg -> 2
  VpaddMask4x32Mem -> 2
  VpaddMask2x64Reg -> 3
  VpaddMask2x64Mem -> 3
  Jo -> 0
  Jno -> 0
  Js -> 0
  Jns -> 0
  Jz -> 0
  Jnz -> 0
  Jc -> 0
  Jnc -> 0
  Jle -> 0

elementCount :: Operator args -> Word
elementCount = \case
  TestReg8Reg8 -> 0
  TestReg16Reg16 -> 0
  TestReg32Reg32 -> 0
  TestReg64Reg64 -> 0
  CmpReg8Reg8 -> 0
  CmpReg16Reg16 -> 0
  CmpReg32Reg32 -> 0
  CmpReg64Reg64 -> 0
  CmpReg64Imm8 -> 0
  CmpReg64Imm32 -> 0
  XorReg8Reg8 -> 0
  XorReg16Reg16 -> 0
  XorReg32Reg32 -> 0
  XorReg64Reg64 -> 0
  SetaReg8 -> 0
  SetaeReg8 -> 0
  SetbReg8 -> 0
  SetbeReg8 -> 0
  SetcReg8 -> 0
  SeteReg8 -> 0
  SetgReg8 -> 0
  SetgeReg8 -> 0
  SetlReg8 -> 0
  SetleReg8 -> 0
  SetnaReg8 -> 0
  SetnaeReg8 -> 0
  SetnbReg8 -> 0
  SetnbeReg8 -> 0
  SetncReg8 -> 0
  SetneReg8 -> 0
  SetngReg8 -> 0
  SetngeReg8 -> 0
  SetnlReg8 -> 0
  SetnleReg8 -> 0
  SetnoReg8 -> 0
  SetnpReg8 -> 0
  SetnsReg8 -> 0
  SetnzReg8 -> 0
  SetoReg8 -> 0
  SetpReg8 -> 0
  SetpeReg8 -> 0
  SetpoReg8 -> 0
  SetsReg8 -> 0
  SetzReg8 -> 0
  NegReg8 -> 0
  NegReg16 -> 0
  NegReg32 -> 0
  NegReg64 -> 0
  Lea16 -> 0
  Lea32 -> 0
  Lea64 -> 0
  MovReg8Imm8 -> 0
  MovReg16Imm16 -> 0
  MovReg32Imm32 -> 0
  MovReg64Imm64 -> 0
  MovReg8Reg8 -> 0
  MovReg16Reg16 -> 0
  MovReg32Reg32 -> 0
  MovReg64Reg64 -> 0
  MovMem8Reg8 -> 0
  MovMem16Reg16 -> 0
  MovMem32Reg32 -> 0
  MovMem64Reg64 -> 0
  MovReg8Mem8 -> 0
  MovReg16Mem16 -> 0
  MovReg32Mem32 -> 0
  MovReg64Mem64 -> 0
  AddAcc8Imm8 -> 0
  AddAcc16Imm16 -> 0
  AddAcc32Imm32 -> 0
  AddAcc64Imm32 -> 0
  AddReg8Reg8 -> 0
  AddReg16Reg16 -> 0
  AddReg32Reg32 -> 0
  AddReg64Reg64 -> 0
  AddReg8Mem8 -> 0
  AddReg16Mem16 -> 0
  AddReg32Mem32 -> 0
  AddReg64Mem64 -> 0
  AddMem8Reg8 -> 0
  AddMem16Reg16 -> 0
  AddMem32Reg32 -> 0
  AddMem64Reg64 -> 0
  AddReg8Imm8 -> 0
  AddReg16Imm8 -> 0
  AddReg32Imm8 -> 0
  AddReg64Imm8 -> 0
  AddReg16Imm16 -> 0
  AddReg32Imm32 -> 0
  AddReg64Imm32 -> 0
  Vpadd64x8Reg -> 6
  Vpadd64x8Mem -> 6
  Vpadd32x8Reg -> 5
  Vpadd32x8Mem -> 5
  Vpadd16x8Reg -> 4
  Vpadd16x8Mem -> 4
  Vpadd32x16Reg -> 5
  Vpadd32x16Mem -> 5
  Vpadd16x16Reg -> 4
  Vpadd16x16Mem -> 4
  Vpadd8x16Reg -> 3
  Vpadd8x16Mem -> 3
  Vpadd16x32Reg -> 4
  Vpadd16x32Mem -> 4
  Vpadd8x32Reg -> 3
  Vpadd8x32Mem -> 3
  Vpadd4x32Reg -> 2
  Vpadd4x32Mem -> 2
  Vpadd8x64Reg -> 3
  Vpadd8x64Mem -> 3
  Vpadd4x64Reg -> 2
  Vpadd4x64Mem -> 2
  Vpadd2x64Reg -> 1
  Vpadd2x64Mem -> 1
  VpaddMask16x8Reg -> 4
  VpaddMask16x8Mem -> 4
  VpaddMask8x16Reg -> 3
  VpaddMask8x16Mem -> 3
  VpaddMask4x32Reg -> 2
  VpaddMask4x32Mem -> 2
  VpaddMask2x64Reg -> 1
  VpaddMask2x64Mem -> 1
  Jo -> 0
  Jno -> 0
  Js -> 0
  Jns -> 0
  Jz -> 0
  Jnz -> 0
  Jc -> 0
  Jnc -> 0
  Jle -> 0
